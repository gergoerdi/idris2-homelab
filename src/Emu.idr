module Emu

import Data.IORef
import JS.Buffer
import Web.Interval

import CPU
import MemoryMap
import Core
import HL2.Machine
import HL2.MemoryMap
import HL2.Clock
import Keyboard

-- The real stuff is in `start`
main : IO ()
main = pure ()

covering
untilIO : acc -> (acc -> IO (Either acc r)) -> IO r
untilIO acc0 step = fromPrim $ go acc0
  where
    go : acc -> PrimIO r
    go acc w =
      let MkIORes (Left acc') w' = toPrim (step acc) w
            | MkIORes (Right res) w' => MkIORes res w'
      in go acc' w'

covering
untilM : IO Bool -> IO ()
untilM step = fromPrim go
  where
    go : PrimIO ()
    go w =
      let MkIORes finished w' = toPrim step w
      in if finished then MkIORes () w' else go w'

%foreign "javascript:lambda: vram => startVideo(vram)"
prim__startVideo : UInt8Array -> PrimIO ()

startVideo : UInt8Array -> IO ()
startVideo vram = primIO $ prim__startVideo vram

record St where
  constructor MkSt
  clock : Ticks
  newFrame : Bool
  videoRunning : Bool

tickClock : (Ticks -> (Int, Ticks)) -> St -> St
tickClock f s =
  let (frames_finished, clock') = f s.clock
  in { newFrame $= (|| frames_finished > 0), clock := clock' } s

public export
%export "javascript:start"
start : ArrayBuffer -> IO (IO ())
start mainBuf = do
  mainROM <- pure $ cast mainBuf
  mainRAM <- newRAM 0x4000
  videoRAM <- newRAM 0x400

  cell <- newIORef $ MkSt
    { clock = startTime
    , newFrame = False
    , videoRunning = False
    }

  let get : HasIO io => (St -> a) -> io a
      get f = liftIO $ f <$> readIORef cell

      modify : HasIO io => (St -> St) -> io ()
      modify f = liftIO $ do
        s <- readIORef cell
        writeIORef cell (f s)

  let untilNewFrame : IO () -> IO ()
      untilNewFrame act = do
        modify { newFrame := False }
        untilM $ act *> get newFrame

  let machine : Machine IO
      machine = MkMachine
        { mainROM = mainROM
        , mainRAM = mainRAM
        , videoRAM = videoRAM
        , keyState = pure (\_ => False) -- ?keyState
        , videoRunning = get videoRunning
        , videoOn = modify $ { videoRunning := True } . tickClock waitLine
        , videoOff = modify { videoRunning := False }
        , tapeIn = pure False
        , tapeOut = pure ()
        }

  startVideo videoRAM

  cpu <- initCPU $ memoryMappedOnly $ HL2.MemoryMap.memoryMap machine
  let runFrame : IO ()
      runFrame = do
        video_running <- get videoRunning
        when video_running $ triggerNMI cpu
        untilNewFrame $ do
          cnt <- liftIO $ runInstruction cpu
          modify $ tickClock $ tick cnt
  interval_id <- setInterval (cast $ 1000 `div` FPS) runFrame
  pure $ clearInterval interval_id

module Emu

import Data.IORef
import JS.Util
import JS.Buffer
import Web.Interval

import UI

import Emu.CPU
import Emu.MemoryMap
import Emu.Core
import Emu.HL2.Machine
import Emu.HL2.MemoryMap
import Emu.HL2.Clock
import Emu.Keyboard

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

tickClock : Int -> St -> St
tickClock k s =
    let (clock', frames_finished) = tick k s.clock
    in { newFrame $= (|| frames_finished > 0), clock := clock' } s

waitLine : St -> St
waitLine s = tickClock (nextLine s.clock) s

public export
%export "javascript:startEmu"
startEmu : ArrayBuffer -> IO ()
startEmu mainBuf = do
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

  keyState <- newIORef empty
  let sinkKeys = liftIO . writeIORef keyState

  let machine : Machine IO
      machine = MkMachine
        { mainROM = mainROM
        , mainRAM = mainRAM
        , videoRAM = videoRAM
        , keyState = readIORef keyState
        , videoRunning = get videoRunning
        , videoOn = modify $ { videoRunning := True } . waitLine
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
          modify $ tickClock cnt
  _ <- setInterval (cast $ 1000 `div` FPS) runFrame
  startUI sinkKeys

module Main

import Web.MVC
import Web.MVC.Animate
import Web.MVC.Http
import JS.Array
import JS.Buffer
import Data.IORef

import HL2.Clock
import Ev
import Core
import CPU
import Keyboard
import MemoryMap
import HL2.Machine
import HL2.MemoryMap

%default total

-- The real stuff is in `runUI`
main : IO ()
main = pure ()

record St where
  constructor MkSt
  clock : Ticks
  frameDone : Bool
  videoRunning : Bool
  videoRunningCell : IORef Bool

withMachine : CPU -> Machine m -> Cmd ev -> Cmd (MachineEv m ev)
withMachine cpu machine = map (Run cpu machine)

display : Main.St -> Cmd Ev.Ev
display s = neutral

tickClock : (Ticks -> (Int, Ticks)) -> Main.St -> Main.St
tickClock f s =
  let (frames_finished, clock') = f s.clock
  in { frameDone $= (|| frames_finished > 0), clock := clock' } s

updateMain : Ev.Ev -> Main.St -> Main.St
updateMain ev = case ev of
  Tick n => tickClock $ tick (cast n)
  NewFrame => { frameDone := False }
  VideoOff => { videoRunning := False }
  VideoOn => { videoRunning := True } . tickClock waitLine

update : MachineEv m Ev -> Main.St -> Main.St
update (Init _) = id
update (Run cpu machine ev) = updateMain ev

dataFile : String -> String
dataFile s = "../data/hl2/" <+> s

tapeFile : String -> String
tapeFile s = "../image/hl2/" <+> s

viewMain : CPU -> Machine IO -> Ev.Ev -> Main.St -> Cmd Ev.Ev
viewMain cpu machine ev s = case ev of
  NewFrame => batch
    [ cmdIf s.videoRunning $ liftIO_ $ triggerNMI cpu
    , pure (Tick 0)
    , display s
    ]
  Tick _ => cmdIf (not s.frameDone) $ liftIO $ do
    Tick . cast <$> runInstruction cpu
  VideoOff => liftIO_ $ writeIORef s.videoRunningCell False
  VideoOn => liftIO_ $ writeIORef s.videoRunningCell True

covering
view : MachineEv IO Ev -> Main.St -> Cmd (MachineEv IO Ev)
view (Init partialMachine) = \s => C $ \queueEvent => do
  cpu_cell <- newIORef Nothing
  let machine : Machine IO
      queueEvent' ev = do
        Just cpu <- readIORef cpu_cell
          | Nothing => pure ()
        runJS $ queueEvent $ Run cpu machine $ ev
      machine = { videoOn := queueEvent' VideoOn, videoOff := queueEvent' VideoOff } partialMachine
  cpu <- initCPU $ memoryMappedOnly $ HL2.MemoryMap.memoryMap machine
  writeIORef cpu_cell $ Just cpu
  flip run queueEvent $ withMachine cpu machine $ batch
    [ NewFrame `every` 20
    ]
view (Run cpu machine ev) = map (Run cpu machine) . viewMain cpu machine ev

partial
untilIO : acc -> (acc -> IO (Either acc r)) -> IO r
untilIO acc0 step = fromPrim $ go acc0
  where
    go : acc -> PrimIO r
    go acc w =
      let MkIORes (Left acc') w' = toPrim (step acc) w
            | MkIORes (Right res) w' => MkIORes res w'
      in go acc' w'

%foreign "javascript:lambda: vram => startVideo(vram)"
prim__startVideo : UInt8Array -> PrimIO ()

startVideo : UInt8Array -> IO ()
startVideo vram = primIO $ prim__startVideo vram

%foreign "javascript:lambda: () => code => (keystate[code] && 1) || 0"
prim__keyState : PrimIO KeyState

keyState : HasIO io => io KeyState
keyState = primIO prim__keyState

public export
covering
%export "javascript:startUI"
startUI : ArrayBuffer -> PrimIO ()
startUI mainBuf = toPrim $ do
  mainROM <- pure $ cast mainBuf
  mainRAM <- newRAM 0x4000
  videoRAM <- newRAM 0x400

  videoRunningCell <- newIORef False
  let partialMachine = MkMachine
        { mainROM = mainROM
        , mainRAM = mainRAM
        , videoRAM = videoRAM
        , keyState = keyState
        , videoRunning = readIORef videoRunningCell
        , videoOn = ()
        , videoOff = ()
        }

  startVideo videoRAM
  runMVC update view (putStrLn . dispErr) (Init partialMachine) $ MkSt
    { clock = startTime
    , frameDone = False
    , videoRunning = False
    , videoRunningCell = videoRunningCell
    }

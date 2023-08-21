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

withCPU : CPU -> Cmd Ev -> Cmd CPUEv
withCPU cpu cmd = C $ \handler => run cmd (handler . Run cpu)

content : St -> Node Ev
content s =
  div []
    [ button [onClick NewFrame] [Text "Run for one frame"]
    , p [] [Text $ show s.clock]
    ]

tickClock : (Ticks -> (Int, Ticks)) -> Main.St -> Main.St
tickClock f s =
  let (frames_finished, clock') = f s.clock
  in { frameDone $= (|| frames_finished > 0), clock := clock' } s

export
update : CPUEv -> St -> St
update Init = id
update (Run cpu ev) = case ev of
  Tick n => tickClock $ tick (cast n)
  NewFrame => { frameDone := False }
  VideoOff => { videoRunning := False }
  VideoOn => { videoRunning := True } . tickClock waitLine

dataFile : String -> String
dataFile s = "../data/hl2/" <+> s

tapeFile : String -> String
tapeFile s = "../image/hl2/" <+> s

view : Machine IO -> CPUEv -> St -> Cmd CPUEv
view machine Init s = C $ \queueEvent => do
  cpu <- liftIO $ do
    cell <- newIORef Nothing
    let core = memoryMappedOnly $ HL2.MemoryMap.memoryMap {machine = machine} $ \ev => do
      Just cpu <- readIORef cell
        | Nothing => pure ()
      runJS $ queueEvent (Run cpu ev)
    cpu <- liftIO $ initCPU core
    writeIORef cell $ Just cpu
    pure cpu
  run (withCPU cpu $ NewFrame `every` 20) queueEvent
view machine (Run cpu ev) s = withCPU cpu $ case ev of
  NewFrame => batch
    [ cmdIf s.videoRunning $ liftIO_ $ triggerNMI cpu
    , pure (Tick 0)
    ]
  Tick _ => cmdIf (not s.frameDone) $ liftIO $ do
    Tick . cast <$> runInstruction cpu
  VideoOff => liftIO_ $ writeIORef s.videoRunningCell False
  VideoOn => liftIO_ $ writeIORef s.videoRunningCell True

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
  let machine = MkMachine
        { mainROM = mainROM
        , mainRAM = mainRAM
        , videoRAM = videoRAM
        , keyState = keyState
        , videoRunning = readIORef videoRunningCell
        }

  startVideo videoRAM
  runMVC update (view machine) (putStrLn . dispErr) Init $ MkSt
    { clock = startTime
    , frameDone = False
    , videoRunning = False
    , videoRunningCell = videoRunningCell
    }

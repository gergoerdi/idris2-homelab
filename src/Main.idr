module Main

import Web.MVC
import Web.MVC.Http
import JS.Array
import Data.IORef

import HL2.Clock
import Ev
import Core
import CPU
import Keyboard
import HL2.Machine
import HL2.MemoryMap

%default total

record St where
  constructor MkSt
  clock : Time
  frameDone : Bool
  videoRunning : Bool

withCPU : CPU -> Cmd Ev -> Cmd CPUEv
withCPU cpu cmd = C $ \handler => run cmd (handler . Run cpu)

content : St -> Node Ev
content s =
  div []
    [ button [onClick Step] [Text "Run for one frame"]
    , p [] [Text $ show s.clock]
    ]

display : St -> Cmd Ev
display s = child (the (Ref HTML.Tag.Div) $ Id "idris") $ content s

export
update : CPUEv -> St -> St
update Init = id
update (Run cpu ev) = case ev of
  Step => id
  Tick n => \s =>
    let (frameDone, clock') = tick (cast n) s.clock
    in { clock := clock', frameDone $= (|| frameDone) } s
  NewFrame => { frameDone := False }
  VideoOff => { videoRunning := False }
  VideoOn => \s =>
    let (frameDone, clock') = waitLine s.clock
    in { videoRunning := True, clock := clock', frameDone $= (|| frameDone) } s

dataFile : String -> String
dataFile s = "../data/hl2/" <+> s

tapeFile : String -> String
tapeFile s = "../image/hl2/" <+> s

view : Machine IO -> CPUEv -> St -> Cmd CPUEv
view machine Init s = C $ \queueEvent => do
  cpu <- liftIO $ do
    cell <- newIORef Nothing
    let core = memoryMappedOnly $ HL2.MemoryMap.memoryMap {machine = machine} s.videoRunning $ \ev => do
      Just cpu <- readIORef cell
        | Nothing => pure ()
      runJS $ queueEvent (Run cpu ev)
    cpu <- liftIO $ initCPU core
    writeIORef cell $ Just cpu
    pure cpu
  queueEvent $ Run cpu NewFrame
view machine (Run cpu ev) s = withCPU cpu $ case ev of
  NewFrame => liftIO_ (when s.videoRunning $ triggerNMI cpu) <+> display s
  Step => if s.frameDone then display s <+> pure NewFrame else C $ \queueEvent => do
    cnt <- liftIO $ runInstruction cpu
    queueEvent $ Tick (cast cnt)
    queueEvent $ Step
  Tick _ => neutral
  VideoOff => neutral
  VideoOn => neutral

partial
untilIO : acc -> (acc -> IO (Either acc r)) -> IO r
untilIO acc0 step = fromPrim $ go acc0
  where
    go : acc -> PrimIO r
    go acc w =
      let MkIORes (Left acc') w' = toPrim (step acc) w
            | MkIORes (Right res) w' => MkIORes res w'
      in go acc' w'

partial
fillRAM : el -> Array el -> IO ()
fillRAM v arr = do
  n <- sizeIO arr
  untilIO n $ \i => case i of
    0 => pure $ Right ()
    i => do
      let i' = i - 1
      writeIO arr i' v
      pure $ Left i'

%foreign "javascript:lambda: vram => startVideo(vram)"
prim__startVideo : Array Bits8 -> PrimIO ()

startVideo : Array Bits8 -> IO ()
startVideo vram = primIO $ prim__startVideo vram

public export
covering
%export "javascript:startUI"
startUI : ArrayBuffer -> PrimIO ()
startUI mainBuf = toPrim $ do
  mainROM <- pure $ the UInt8Array (cast mainBuf)
  mainRAM <- newArrayIO 0x4000
  fillRAM 0x00 mainRAM
  videoRAM <- newArrayIO 0x400
  fillRAM 0x00 videoRAM
  let machine = MkMachine
        { mainROM = mainROM
        , mainRAM = mainRAM
        , videoRAM = videoRAM
        , keyState = the (IO KeyState) $ pure $ \code => False
        }
  startVideo videoRAM
  runMVC update (view machine) (putStrLn . dispErr) Init $ MkSt
    { clock = startTime
    , frameDone = False
    , videoRunning = False
    }

covering
main : IO ()
main = pure ()

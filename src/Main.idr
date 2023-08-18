module Main

import Web.MVC
import Web.MVC.Http
import JS.Array

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
  cpu : Maybe CPU
  clock : Time
  frameDone : Bool

content : St -> Node Ev
content s =
  div []
    [ button [onClick Step] [Text "Run for one frame"]
    , p [] [Text $ show s.clock]
    ]

export
update : Ev -> St -> St
update Init = id
update Step = id
update (GotCPU cpu) = { cpu := Just cpu }
update (Tick n) = \s => let (frameDone, clock') = tick n s.clock in
  { clock := clock', frameDone $= (|| frameDone) } s
update NewFrame = { frameDone := False }

dataFile : String -> String
dataFile s = "../data/hl2/" <+> s

tapeFile : String -> String
tapeFile s = "../image/hl2/" <+> s

view : Machine IO -> Ev -> St -> Cmd Ev
view machine Init s = C $ \queueEvent => do
  let core = memoryMappedOnly $ HL2.MemoryMap.memoryMap {machine = machine} (runJS . queueEvent)
  cpu <- liftIO $ initCPU core
  queueEvent $ GotCPU cpu
view machine Step s = if s.frameDone then (child Body $ content s) <+> pure NewFrame else C $ \queueEvent => do
  let cpu = fromMaybe (assert_total $ idris_crash "cpu") s.cpu
  cnt <- liftIO $ runInstruction cpu
  queueEvent $ Tick (cast cnt)
  queueEvent Step
view machine _ s = child Body $ content s

-- view LoadMainROM s = request GET [] (dataFile "rom.bin") Empty ?e1 Nothing
-- view LoadCharROM s = request GET [] (dataFile "charset.bin") Empty ?e2 Nothing

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

public export
covering
%export "javascript:startUI"
startUI : ArrayBuffer -> ArrayBuffer -> IO ()
startUI mainBuf charBuf = do
  mainROM <- arrayDataFrom $ the UInt8Array (cast mainBuf)
  charROM <- arrayDataFrom $ the UInt8Array (cast charBuf)
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
  runMVC update (view machine) (putStrLn . dispErr) Init $ MkSt
    { cpu = Nothing
    , clock = startTime
    , frameDone = False
    }

covering
main : IO ()
main = pure ()

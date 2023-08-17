module Main

import Web.MVC
import Web.MVC.Http
import JS.Array

import Ev
import Core
import CPU
import HL2.Machine
import HL2.MemoryMap

%default total

record St where
  constructor MkSt
  cpu : Maybe CPU
  romAddrs : List Bits16

content : St -> Node Ev
content s =
  div []
    [ p [] [Text $ show s.romAddrs]
    -- , button [onClick LoadMainROM] [Text "Load ROM"]
    ]

export
update : Ev -> St -> St
update Init = id
update (GotCPU cpu) = { cpu := Just cpu }
update (RomSpy addr) = { romAddrs $= (addr::) }

dataFile : String -> String
dataFile s = "../data/hl2/" <+> s

tapeFile : String -> String
tapeFile s = "../image/hl2/" <+> s

view : Machine -> Ev -> St -> Cmd Ev
view machine Init s = C $ \h => do
  let core = memoryMappedOnly $ HL2.MemoryMap.memoryMap {machine = machine} (runJS . h)
  cpu <- liftIO $ initCPU core
  h $ GotCPU cpu
view machine (GotCPU _) s = C $ \h => do
  let cpu = fromMaybe (assert_total $ idris_crash "cpu") s.cpu
  cnt <- liftIO $ runInstruction cpu
  ($ h) . run $
    child Body $ content s
view machine _ s = child Body $ content s

-- view LoadMainROM s = request GET [] (dataFile "rom.bin") Empty ?e1 Nothing
-- view LoadCharROM s = request GET [] (dataFile "charset.bin") Empty ?e2 Nothing

public export
covering
startUI : ArrayBuffer -> ArrayBuffer -> IO ()
startUI mainBuf charBuf = do
  mainROM <- arrayDataFrom $ the UInt8Array (cast mainBuf)
  charROM <- arrayDataFrom $ the UInt8Array (cast charBuf)
  mainRAM <- newArrayIO 0
  videoRAM <- newArrayIO 0
  let machine = MkMachine
        { mainROM = mainROM
        , mainRAM = mainRAM
        , videoRAM = videoRAM
        }
  runMVC update (view machine) (putStrLn . dispErr) Init $
    MkSt{ cpu = Nothing, romAddrs = [] }

%foreign "javascript:lambda: f => () => prepareUI(f)"
prepareUI : (ArrayBuffer -> ArrayBuffer -> IO ()) -> IO ()

covering
main : IO ()
main = prepareUI startUI

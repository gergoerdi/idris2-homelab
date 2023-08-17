module Core

import Web.MVC
import MemoryMap

public export
record Core where
  constructor MkCore
  readMem  : Bits16 -> IO Bits8
  writeMem : Bits16 -> Bits8 -> IO ()
  readIO   : Bits8 -> IO Bits8
  writeIO  : Bits8 -> Bits8 -> IO ()

public export
memoryMappedOnly : MemoryUnit IO Bits16 Bits8 -> Core
memoryMappedOnly mem = MkCore
  { readMem = read mem
  , writeMem = write mem
  , readIO = \_ => pure 0x00
  , writeIO = \_, _ => pure ()
  }

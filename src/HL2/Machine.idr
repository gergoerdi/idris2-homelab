module HL2.Machine

import Data.Buffer.Index
import Data.ByteVect
import Keyboard

import JS.Array
import JS.Buffer

public export
0 ROM : Nat -> Type
-- ROM size = ByteVect size
ROM size = UInt8Array

public export
0 RAM : Nat -> Type
RAM size = UInt8Array

%foreign "javascript:lambda: size => new Uint8Array(size)"
prim__newUInt8Array : Bits32 -> PrimIO UInt8Array

export
newRAM : HasIO io => (size : Nat) -> io (RAM size)
newRAM size = primIO $ prim__newUInt8Array (cast size)

public export
record Machine m where
  constructor MkMachine

  mainROM : ROM 0x2000
  mainRAM : RAM 0x4000
  videoRAM : RAM 0x400

  keyState : m KeyState

  videoRunning : m Bool
  videoOn : m ()
  videoOff : m ()

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
RAM size = Array Bits8

public export
record Machine m where
  constructor MkMachine

  mainROM : ROM 0x2000
  mainRAM : RAM 0x4000
  videoRAM : RAM 0x400

  keyState : m KeyState
  -- videoOn_ : m ()
  -- videoOff_ : m ()

  render : m ()

public export
mainROM : (machine: Machine m) => ROM 0x2000
mainROM = machine.mainROM

public export
mainRAM : (machine: Machine m) => RAM 0x4000
mainRAM = machine.mainRAM

public export
videoRAM : (machine: Machine m) => RAM 0x400
videoRAM = machine.videoRAM

public export
keyState : (machine: Machine m) => m KeyState
keyState = machine.keyState

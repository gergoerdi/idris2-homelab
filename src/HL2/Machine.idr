module HL2.Machine

import Data.Buffer.Index
import Data.ByteVect
import Keyboard

import JS.Array

public export
0 ROM : Nat -> Type
-- ROM size = ByteVect size
ROM size = Array Bits8

public export
0 RAM : Nat -> Type
RAM size = Array Bits8

public export
record Machine where
  constructor MkMachine

  mainROM : ROM 0x2000
  mainRAM : RAM 0x4000
  videoRAM : RAM 0x400

  -- keyState_ : KeyState
  -- videoOn_ : m ()
  -- videoOff_ : m ()

public export
mainROM : Machine => ROM 0x2000
mainROM = mainROM %search

public export
mainRAM : Machine => RAM 0x4000
mainRAM = mainRAM %search

public export
videoRAM : Machine => RAM 0x400
videoRAM = videoRAM %search

-- public export
-- keyState : Machine => KeyState
-- keyState = keyState_ %search

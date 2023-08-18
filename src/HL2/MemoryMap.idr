module HL2.MemoryMap

import Data.So
import Ev
import MemoryMap
import Data.Buffer.Index
import Data.ByteVect

import Keyboard
import HL2.Keyboard
import HL2.Machine

import JS.Array

addressByte : Index 256 -> Bits8
addressByte (Element addr _) = cast addr

-- interface Monad m => HasMemory m where
--   mainROM : ByteVect 0x2000
--   mainRAM : Array Bits8

-- interface Monad m => HasVideo m where
--   videoOn : m ()
--   videoOff : m ()
--   videoRAM : Array Bits8

interface Monad m => HasTape m where
  tapeOut : m ()
  tapeIn : m Bool

-- interface Monad m => HasKeyboard m where
--   keyState : m KeyState

public export
memoryMap : HasIO m => (machine: Machine m) => (queueEvent : Ev -> m ()) -> MemoryUnit m Bits16 Bits8
memoryMap queueEvent = contramap cast $ memoryMap
  [ MkMapEntry{ from = 0x0000, to = 0x1fff, unit = theROM } -- rom mainROM }
  , MkMapEntry{ from = 0x3800, to = 0x39ff, unit = unconnected 0x00 } -- TODO: reset button
  , MkMapEntry{ from = 0x3a00, to = 0x3aff, unit = readOnly $ \addr => do
                     keyState <- machine.keyState
                     pure $ keyboardByte keyState (addressByte addr) }
  , MkMapEntry{ from = 0x3b00, to = 0x3bff, unit = unconnected 0x00 } -- TODO: ??
  -- , MkMapEntry{ from = 0x3c00, to = 0x3dff, unit = trigger 0xff tapeOut }
  -- , MkMapEntry{ from = 0x3e00, to = 0x3eff, unit = trigger 0xff videoOff }
  -- , MkMapEntry{ from = 0x3f00, to = 0x3fff, unit = trigger 0xff videoOn }
  , MkMapEntry{ from = 0x4000, to = 0x7fff, unit = ram machine.mainRAM }
  , MkMapEntry{ from = 0xc000, to = 0xc3ff, unit = ram machine.videoRAM }
  -- , MkMapEntry{ from = 0xe000, to = 0xffff, unit = readOnly video_scan }
  ]
  (unconnected 0xff)
  where
    theROM : MemoryUnit m (Addr 0x0000 0x1fff) Bits8
    theROM = MkMemoryUnit
      { read = \addr@(Element i _) => do
             -- queueEvent Inside
             read (rom machine.mainROM) addr
      , write = write (rom machine.mainROM)
      }

  -- where
  --   video_scan : Addr 0xe000 0xffff -> m Bits8
  --   video_scan addr = do
  --     tape_signal <- tapeIn
  --     pure $ if tape_signal then 0xff else 0x00

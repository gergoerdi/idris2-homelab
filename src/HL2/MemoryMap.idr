module HL2.MemoryMap

import Data.So
import Ev
import MemoryMap
import Data.ByteVect

import CPU
import Keyboard
import HL2.Keyboard
import HL2.Machine

import JS.Array
import JS.Buffer

addressByte : Addr 256 -> Bits8
addressByte (Element addr _) = cast addr

public export
memoryMap : HasIO m => (machine: Machine m) => Bool -> (queueEvent : Ev -> m ()) -> MemoryUnit m Bits16 Bits8
memoryMap videoRunning queueEvent = contramap cast $ memoryMap
  [ MkMapEntry{ from = 0x0000, to = 0x1fff, unit = rom machine.mainROM }
  , MkMapEntry{ from = 0x3800, to = 0x39ff, unit = unconnected 0x00 } -- TODO: reset button
  , MkMapEntry{ from = 0x3a00, to = 0x3aff, unit = readOnly $ \addr => do
                     keyState <- machine.keyState
                     pure $ keyboardByte keyState (addressByte addr) }
  , MkMapEntry{ from = 0x3b00, to = 0x3bff, unit = unconnected 0x00 } -- TODO: ??
  , MkMapEntry{ from = 0x3c00, to = 0x3dff, unit = trigger 0xff tapeOut }
  , MkMapEntry{ from = 0x3e00, to = 0x3eff, unit = trigger 0xff videoOff }
  , MkMapEntry{ from = 0x3f00, to = 0x3fff, unit = trigger 0xff videoOn }
  , MkMapEntry{ from = 0x4000, to = 0x7fff, unit = ram machine.mainRAM }
  , MkMapEntry{ from = 0xc000, to = 0xc3ff, unit = ram machine.videoRAM }
  , MkMapEntry{ from = 0xe000, to = 0xffff, unit = readOnly videoScan }
  ]
  (unconnected 0xff)
  where
    videoOff = queueEvent VideoOff
    videoOn = queueEvent VideoOn

    tapeOut : m ()
    tapeOut = pure () -- TODO

    tapeIn : Bits8
    tapeIn = 0x00 -- TODO

    videoScan : AddrFromTo 0xe000 0xffff -> m Bits8
    videoScan (Element i _) = pure $
        if not videoRunning then tapeIn else
        if i `mod` 40 == 0 then 0xff else
        0x3f

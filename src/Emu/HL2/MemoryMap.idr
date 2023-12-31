module Emu.HL2.MemoryMap

import Data.Prim.Bits16

import Emu.MemoryMap
import Emu.Keyboard
import Emu.HL2.Keyboard
import Emu.HL2.Machine

addressByte : Addr 255 -> Bits8
addressByte (Element addr _) = cast addr

public export
memoryMap : HasIO m => Machine m -> MemoryUnit m Bits16 Bits8
memoryMap machine = contramap cast $ memoryMap
  [ MkMapEntry{ from = 0x0000, to = 0x1fff, unit = rom machine.mainROM }
  , MkMapEntry{ from = 0x3800, to = 0x39ff, unit = unconnected 0x00 } -- TODO: reset button
  , MkMapEntry{ from = 0x3a00, to = 0x3aff, unit = readOnly keyboard }
  , MkMapEntry{ from = 0x3b00, to = 0x3bff, unit = unconnected 0x00 } -- TODO: ??
  , MkMapEntry{ from = 0x3c00, to = 0x3dff, unit = trigger 0xff machine.tapeOut }
  , MkMapEntry{ from = 0x3e00, to = 0x3eff, unit = trigger 0xff machine.videoOff }
  , MkMapEntry{ from = 0x3f00, to = 0x3fff, unit = trigger 0xff machine.videoOn }
  , MkMapEntry{ from = 0x4000, to = 0x7fff, unit = ram machine.mainRAM }
  , MkMapEntry{ from = 0xc000, to = 0xc3ff, unit = ram machine.videoRAM }
  , MkMapEntry{ from = 0xe000, to = 0xffff, unit = readOnly videoScan }
  ]
  (unconnected 0xff)
  where
    tapeIn : m Bits8
    tapeIn = pure $ if !machine.tapeIn then 0xff else 0x00

    videoScan : AddrFromTo 0xe000 0xffff -> m Bits8
    videoScan (Element i _) = do
      video_running <- machine.videoRunning
      if not video_running then tapeIn else
        if i `mod` 40 == 0 then pure 0xff else
          pure 0x3f

    keyboard : AddrFromTo 0x3a00 0x3aff -> m Bits8
    keyboard addr = do
      keyState <- machine.keyState
      pure $ keyboardByte keyState (addressByte addr)

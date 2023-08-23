module Emu.HL2.Keyboard

import Emu.Keyboard
import Data.Buffer.Index
import Data.Vect
import Data.Bits.Fast

keymap : Vect 8 (Vect 8 (Maybe KeyCode))
keymap =
  [ [ Just "ShiftLeft", Nothing,            Nothing,       Nothing,       Nothing,       Just "ShiftRight",  Nothing,       Nothing          ]
  , [ Just "Space",    Just "BracketRight", Nothing,       Just "Equal",  Nothing,       Just "Enter",       Nothing,       Just "Backslash" ]
  , [ Just "Digit0",   Just "Digit1",       Just "Digit2", Just "Digit3", Just "Digit4", Just "Digit5",      Just "Digit6", Just "Digit7"    ]
  , [ Just "Digit8",   Just "Digit9",       Just "Minus",  Just "Quote",  Just "Comma",  Just "BracketLeft", Just "Period", Just "Slash"     ]
  , [ Nothing,         Just "KeyA",         Just "KeyB",   Just "KeyC",   Just "KeyD",   Just "KeyE",        Just "KeyF",   Just "KeyG"      ]
  , [ Just "KeyH",     Just "KeyI",         Just "KeyJ",   Just "KeyK",   Just "KeyL",   Just "KeyM",        Just "KeyN",   Just "KeyO"      ]
  , [ Just "KeyP",     Just "KeyQ",         Just "KeyR",   Just "KeyS",   Just "KeyT",   Just "KeyU",        Just "KeyV",   Just "KeyW"      ]
  , [ Just "KeyX",     Just "KeyY",         Just "KeyZ",   Nothing,       Nothing,       Just "Semicolon",   Nothing,       Nothing          ]
  ]

readRow : KeyState -> Vect 8 (Maybe KeyCode) -> Bits8
readRow keyState = foldl (\val, mb_code => readKey mb_code $ val `shiftR` 1) 0x00
  where
    readKey : Maybe KeyCode -> Bits8 -> Bits8
    readKey code val = if maybe False keyState code then val else val `setBit` 7

public export
keyboardByte : KeyState -> Bits8 -> Bits8
keyboardByte keyState addr = fst $ foldl f (0xff, addr) keymap
  where
    f : (Bits8, Bits8) -> Vect 8 (Maybe KeyCode) -> (Bits8, Bits8)
    f (val, addr) row = (val', addr')
      where
        val' = if addr `testBit` 0 then val else val .&. readRow keyState row
        addr' = addr `shiftR` 1

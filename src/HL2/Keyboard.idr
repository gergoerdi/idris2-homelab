module HL2.Keyboard

import Keyboard
import Data.Buffer.Index
import Data.Vect

keymap : Vect 8 (Vect 8 (Maybe String))
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

-- readRow : KeyState -> Vect 8 (Maybe String) -> Bits8
-- readRow keyState row = ?rr

-- -- function read_row(keystate, row) {
-- --     let val = 0;
-- --     for (key of row) {
-- --         val >>= 1;
-- --         if (!(key && keystate[key]))
-- --             val |= 0x80;
-- --     }
-- --     return val;
-- -- }

public export
keyboardByte : KeyState -> Bits8 -> Bits8
keyboardByte keyState addr = 0xff -- TODO

-- -- function keyboard_byte(keystate, addr) {
-- --     let val = 0xff;

-- --     addr &= 0xff;
-- --     for (row of keymap) {
-- --         if ((addr & 1) == 0)
-- --             val &= read_row(keystate, row);
-- --         addr >>= 1;
-- --     }

-- --     return val;
-- -- }

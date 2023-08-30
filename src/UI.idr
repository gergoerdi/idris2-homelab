module UI

import Web.MVC
import Web.MVC.Widget
import Data.IORef

import Emu.Tape
import Emu.Keyboard

import UI.Tape
import UI.Keyboard
import UI.Mutable

%default total

export
widget : (JSIO () -> JSIO ()) -> (KeyState -> JSIO ()) -> Mutable JSIO Deck -> Widget
widget registerFrameListener sinkKeyboard deck =
  Tape.widget registerFrameListener deck <+>
  Keyboard.widget sinkKeyboard

public export
covering
startUI : (JSIO () -> JSIO ()) -> (KeyState -> JSIO ()) -> Mutable JSIO Deck -> IO ()
startUI registerFrameListener sinkKeyboard deck = runWidget (putStrLn . dispErr) $
  UI.widget registerFrameListener sinkKeyboard deck

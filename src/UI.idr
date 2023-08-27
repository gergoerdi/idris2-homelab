module UI

import Web.MVC
-- import Web.MVC.Animate
-- import Web.MVC.Http
-- import JS.Array
-- import JS.Buffer
import Web.MVC.Widget
import Data.IORef

import Emu.Tape
import Emu.Keyboard

import UI.Tape
import UI.Keyboard
import UI.Mutable

%default total

export
addSetup : (widget : Widget) -> Cmd widget.Ev -> Widget
addSetup widget cmd = { setup := \s => cmd <+> widget.setup s } widget

public export
covering
startUI : IORef (JSIO ()) -> (KeyState -> JSIO ()) -> Mutable JSIO Deck -> IO ()
startUI cell sinkKeyboard deck = runWidget (putStrLn . dispErr) w'
  where
    w : Widget
    w = Tape.widget deck <+> Keyboard.widget sinkKeyboard

    w' : Widget
    w' = addSetup w $ C $ \h => writeIORef cell $ h $ Left Tape.NewFrame

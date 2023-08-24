module UI

import Web.MVC
-- import Web.MVC.Animate
-- import Web.MVC.Http
-- import JS.Array
-- import JS.Buffer
-- import Data.IORef

import Emu.Keyboard

import UI.Tape
import UI.Keyboard

%default total

-- The real stuff is in `startUI`
main : IO ()
main = pure ()

record St where
  constructor MkSt
  tape : Tape.St
  keyboard : Keyboard.St

public export
data Ev : Type where
  Init       : Ev
  TapeEv     : Tape.Ev -> Ev
  KeyboardEv : Keyboard.Ev -> Ev

update : UI.Ev -> UI.St -> UI.St
update Init = id
update (TapeEv ev) = { tape $= update ev }
update (KeyboardEv ev) = { keyboard $= update ev }

covering
view : UI.Ev -> UI.St -> Cmd UI.Ev
view Init s = batch
  [ TapeEv <$> Tape.setupEvents s.tape
  , KeyboardEv <$> Keyboard.setupEvents s.keyboard
  ]
view (TapeEv ev) s = TapeEv <$> Tape.display ev s.tape
view (KeyboardEv ev) s = batch
  [ KeyboardEv <$> Keyboard.display ev s.keyboard
  ]

public export
covering
startUI : (KeyState -> JSIO ()) -> IO ()
startUI sinkKeyboard = runMVC update view' (putStrLn . dispErr) Init $ MkSt
    { tape = startTape
    , keyboard = init
    }
  where
    view' = \ev, s => batch
      [ view ev s
      , Keyboard.pub sinkKeyboard s.keyboard
      ]

module UI

import Web.MVC
-- import Web.MVC.Animate
-- import Web.MVC.Http
-- import JS.Array
-- import JS.Buffer
import Data.IORef

import Emu.Tape
import Emu.Keyboard

import UI.Tape
import UI.Keyboard
import UI.Mutable

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
  NewFrame   : Ev
  TapeEv     : Tape.Ev -> Ev
  KeyboardEv : Keyboard.Ev -> Ev

update : UI.Ev -> UI.St -> UI.St
update Init = id
update NewFrame = id
update (TapeEv ev) = { tape $= Tape.update1 ev }
update (KeyboardEv ev) = { keyboard $= Keyboard.update ev }

covering
view : UI.Ev -> UI.St -> Deck -> Cmd UI.Ev
view Init s deck = batch
  [ TapeEv <$> Tape.setupView s.tape deck
  , KeyboardEv <$> Keyboard.setupEvents s.keyboard
  ]
view NewFrame s deck = Tape.updateView s.tape deck
view (TapeEv     ev) s deck = TapeEv     <$> Tape.display     ev s.tape deck
view (KeyboardEv ev) s deck = KeyboardEv <$> Keyboard.display ev s.keyboard

public export
covering
startUI : IORef (UI.Ev -> JSIO ()) -> (KeyState -> JSIO ()) -> Mutable JSIO Deck -> IO ()
startUI cell sinkKeyboard deck = runMVC update view' (putStrLn . dispErr) Init $ MkSt
    { tape = startTape
    , keyboard = init
    }
  where
    updateExt : UI.Ev -> UI.St -> Cmd UI.Ev
    updateExt Init s = C $ \enqueueEvent => writeIORef cell enqueueEvent
    updateExt NewFrame s = viewExternalState deck $ Tape.updateView s.tape
    updateExt (TapeEv     ev) s = updateExternalState deck Tape.update2 ev s.tape
    updateExt (KeyboardEv ev) s = neutral

    view' = \ev, s => batch
      [ updateExt ev s
      , viewExternalState deck $ \deck => batch
          [ view ev s deck
          , Keyboard.pub sinkKeyboard s.keyboard
          ]
      ]

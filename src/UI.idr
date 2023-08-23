module UI

import Web.MVC
-- import Web.MVC.Animate
-- import Web.MVC.Http
-- import JS.Array
-- import JS.Buffer
-- import Data.IORef

import Emu
import UI.Tape
import UI.Ev

%default total

-- The real stuff is in `startUI`
main : IO ()
main = pure ()

record St where
  constructor MkSt
  tape : Tape.St

update : Ev.Ev -> UI.St -> UI.St
update Init = id
update (TapeEv ev) = { tape $= update ev }

covering
view : Ev.Ev -> UI.St -> Cmd Ev.Ev
view Init s = batch
  [ TapeEv <$> Tape.setupEvents s.tape
  ]
view (TapeEv ev) s = TapeEv <$> Tape.display (Just ev) s.tape

public export
covering
startUI : IO ()
startUI = runMVC update view (putStrLn . dispErr) Init $ MkSt
  { tape = startTape
  }

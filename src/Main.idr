module Main

import Web.MVC
-- import Web.MVC.Animate
-- import Web.MVC.Http
-- import JS.Array
-- import JS.Buffer
-- import Data.IORef

import Emu
import Ev

%default total

-- The real stuff is in `startUI`
main : IO ()
main = pure ()

record St where
  constructor MkSt

update : Ev.Ev -> Main.St -> Main.St
update Init = id

dataFile : String -> String
dataFile s = "../data/hl2/" <+> s

tapeFile : String -> String
tapeFile s = "../image/hl2/" <+> s

covering
view : Ev.Ev -> Main.St -> Cmd Ev.Ev
view Init s = batch
  [
  ]

public export
covering
%export "javascript:startUI"
startUI : PrimIO ()
startUI = toPrim $ runMVC update view (putStrLn . dispErr) Init $ MkSt {}

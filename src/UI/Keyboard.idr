module UI.Keyboard

import Emu.Keyboard

import Web.MVC
import Web.Html

%default total

export
data Ev : Type where
  Blur : Ev
  Key : Bool -> KeyCode -> Ev

export
0 St : Type
St = KeyState

public export
init : St
init = empty

public export
update : Ev -> St -> St
update Blur = const empty
update (Key press code) = setKeyState press code

%foreign "javascript:lambda: setOnBlur"
prim__setOnBlur : IO () -> PrimIO ()

public export
setupEvents : St -> Cmd Ev
setupEvents s = batch
  [ attr Body $ Event . KeyDown $ Just . Key True . code
  , attr Body $ Event . KeyUp $ Just . Key False . code
  -- , attr Window $ onBlur Blur -- TODO: runtime type error
  , C $ \enqueueEvent => liftIO $ primIO $ prim__setOnBlur $ runJS . enqueueEvent $ Blur
  ]

public export
pub : (KeyState -> JSIO ()) -> St -> Cmd ev
pub sink s = cmd_ $ sink s

public export
display : Ev -> St -> Cmd Ev
display Blur s = neutral
display (Key press code) s = neutral
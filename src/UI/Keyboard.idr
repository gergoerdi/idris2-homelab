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
update Blur = id
update (Key press code) = setKeyState press code

public export
setupEvents : St -> Cmd Ev
setupEvents s = batch
  [ attr Body $ onBlur Blur
  , attr Body $ Event . KeyDown $ Just . Key True . code
  , attr Body $ Event . KeyUp $ Just . Key False . code
  ]

public export
pub : (KeyState -> JSIO ()) -> St -> Cmd ev
pub sink s = cmd_ $ sink s

public export
display : Ev -> St -> Cmd Ev
display Blur s = neutral
display (Key press code) s = neutral

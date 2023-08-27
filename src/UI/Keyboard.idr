module UI.Keyboard

import Emu.Keyboard

import Web.MVC
import Web.MVC.Event
import Web.Html
import Web.MVC.Widget

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

%foreign "javascript:lambda: (cb) => window.addEventListener('blur', () => cb())"
prim__setOnBlur : IO () -> PrimIO ()

%foreign "javascript:lambda: (cb) => window.addEventListener('keydown', (e) => cb(e)())"
prim__setOnKeyDown : (KeyboardEvent -> IO ()) -> PrimIO ()

%foreign "javascript:lambda: (cb) => window.addEventListener('keyup', (e) => cb(e)())"
prim__setOnKeyUp : (KeyboardEvent -> IO ()) -> PrimIO ()

public export
setupEvents : Cmd Ev
setupEvents = batch
  -- [ attr Window $ Event . KeyDown $ Just . Key True . code
  -- , attr Window $ Event . KeyUp $ Just . Key False . code
  -- , attr Window $ onBlur Blur -- TODO: runtime type error
  [ C $ \enqueueEvent => liftIO $ do
      primIO $ prim__setOnBlur $ runJS . enqueueEvent $ Blur
      primIO $ prim__setOnKeyDown $ runJS . (enqueueEvent . Key True . code <=< keyInfo)
      primIO $ prim__setOnKeyUp $ runJS . (enqueueEvent . Key False . code <=< keyInfo)
  ]

public export
pub : (KeyState -> JSIO ()) -> St -> Cmd ev
pub sink s = cmd_ $ sink s

public export
display : Ev -> St -> Cmd Ev
display Blur s = neutral
display (Key press code) s = neutral

export
widget : (KeyState -> JSIO ()) -> Widget
widget sink = MkWidget
  { St = KeyState
  , Ev = Ev
  , init = empty
  , setup = \_ => setupEvents
  , update = update
  , display = \_, s => cmd_ $ sink s
  }

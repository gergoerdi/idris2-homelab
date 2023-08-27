module UI.Mutable

import Web.MVC
import Web.MVC.Widget

%default total

public export
record Mutable m a where
  constructor MkMutable
  read : m a
  modify : (a -> a) -> m ()

export
viewExternalState : {0 a : Type} -> Mutable JSIO a -> (a -> Cmd ev) -> Cmd ev
viewExternalState mut f = C $ \h => do
  es <- read mut
  run (f es) h

export
updateExternalState : {0 a : Type} -> Mutable JSIO a -> (ev -> st -> a -> a) -> (ev -> st -> Cmd ev')
updateExternalState mut f ev s = cmd_ $ modify mut $ f ev s

public export
mutableWidget :
     {0 a : Type}
  -> (St : Type)
  -> (Ev : Type)
  -> (init : St)
  -> (setup : St -> a -> Cmd Ev)
  -> (update1 : Ev -> St -> St)
  -> (update2 : Ev -> St -> a -> a)
  -> (display : Ev -> St -> a -> Cmd Ev)
  -> (mut : Mutable JSIO a)
  -> Widget
mutableWidget st ev init setup update1 update2 display mut = MkWidget
  { St = st
  , Ev = ev
  , init = init
  , setup = \s => viewExternalState mut $ setup s
  , update = update1
  , display = \ev, s => batch
      [ updateExternalState mut update2 ev s
      , viewExternalState mut $ display ev s
      ]
  }

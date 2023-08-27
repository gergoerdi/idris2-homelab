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

public export
mutableWidget :
     {0 Ext : Type}
  -> (St : Type)
  -> (Ev : Type)
  -> (init : St)
  -> (setup : St -> Ext -> Cmd Ev)
  -> (update : Ev -> St -> (St, Ext -> Ext))
  -> (display : Ev -> St -> Ext -> Cmd Ev)
  -> (mut : Mutable JSIO Ext)
  -> Widget
mutableWidget {Ext = ext} st ev init setup update display mut = MkWidget
  { St = (st, ext -> ext)
  , Ev = ev
  , init = (init, id)
  , setup = \(s, _) => viewExternalState mut $ setup s
  , update = \ev, (s, _) => update ev s
  , display = \ev, (s, updateExt) => batch
      [ cmd_ $ modify mut updateExt
      , viewExternalState mut $ display ev s
      ]
  }

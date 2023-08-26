module UI.Mutable

import Web.MVC

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

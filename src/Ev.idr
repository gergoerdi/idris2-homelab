module Ev

import CPU
import Web.MVC

-- %default total

public export
data Ev : Type where
  Init : Ev
  GotCPU : CPU -> Ev

-- public export
-- record Cmd' ev a where
--   constructor C'
--   runCmd' : (handler : ev -> JSIO ()) -> JSIO a

-- public export
-- Functor (Cmd' ev) where
--   map f cmd = C' $ \h => map f (runCmd' cmd h)

-- public export
-- Applicative (Cmd' ev) where
--   pure x = C' $ \_ => pure x
--   ff <*> fx = C' $ \h => runCmd' ff h <*> runCmd' fx h

-- public export
-- Monad (Cmd' ev) where
--   m >>= k = C' $ \h => do
--     x <- runCmd' m h
--     runCmd' (k x) h

-- public export
-- HasIO (Cmd' ev) where
--   liftIO act = C' $ \h => liftIO act

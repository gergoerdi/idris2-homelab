module HL2.Clock

import Data.Buffer.Index
import Data.Nat
import Control.Monad.Writer
import Data.IORef

public export
data Time : Type where
  Visible : Index 288 -> Index 256 -> Time
  Blank   : Index 6272             -> Time

public export
Show Time where
  show (Visible y x) = "Line " <+> show y <+> ", col " <+> show x
  show (Blank cnt) = "Blank " <+> show cnt

last : {n : Nat} -> Index (S n)
last = Element n reflexive

export
startTime : Time
startTime = Blank last

prev : {n : Nat} -> Index (S n) -> Maybe (Index (S n))
prev (Element 0 _) = Nothing
prev (Element (S n) prf) = Just $ Element n $ lteSuccLeft prf

tick1 : Time -> Maybe Time
tick1 (Visible y x) = case prev x of
  Just x' => pure $ Visible y x'
  Nothing => pure $ Visible !(prev y) last
tick1 (Blank cnt) = pure $ maybe (Visible last last) Blank $ prev cnt

export
tick : Nat -> Time -> (Bool, Time)
tick 0 t = (False, t)
tick (S n) t =
     case tick1 t of
       Nothing => (True, snd $ tick n startTime)
       Just t' => tick n t'

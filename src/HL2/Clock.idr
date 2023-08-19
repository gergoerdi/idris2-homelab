module HL2.Clock

import Data.Buffer.Index
import Data.Nat
import Control.Monad.Writer
import Data.IORef

%default total

public export
0 CPUFreq : Nat
CPUFreq = 4_000_000

public export
0 FPS : Nat
FPS = 50

public export
0 HorizCount : Nat
HorizCount = 256

public export
0 VisibleLines : Nat
VisibleLines = 288

public export
0 FrameCount : Nat
-- FrameCount = (CPUFreq `divNatNZ` FPS) % search -- This blows up typechecking time :/
FrameCount = 80_000

public export
0 BlankCount : Nat
BlankCount = FrameCount `minus` (HorizCount * VisibleLines)

public export
data Time : Type where
  Visible : Index VisibleLines -> Index HorizCount -> Time
  Blank   : Index BlankCount                       -> Time

public export
Show Time where
  show (Visible y x) = "Line " <+> show y <+> ", col " <+> show x
  show (Blank cnt) = "Blank " <+> show cnt

last : {n : Nat} -> Index (S n)
last = Element n reflexive

export
startTime : Time
startTime = Visible last last

prev : {n : Nat} -> Index (S n) -> Maybe (Index (S n))
prev (Element 0 _) = Nothing
prev (Element (S n) prf) = Just $ Element n $ lteSuccLeft prf

tick1 : Time -> Maybe Time
tick1 (Blank cnt) = pure $ Blank !(prev cnt)
tick1 (Visible y x) = pure $ case prev x of
  Just x' => Visible y x'
  Nothing => maybe (Blank last) (\y' => Visible y' last) (prev y)

public export
tick : Nat -> Time -> (Bool, Time)
tick 0 t = (False, t)
tick (S n) t =
     case tick1 t of
       Nothing => (True, snd $ tick n startTime)
       Just t' => tick n t'

public export
waitLine : Time -> (Bool, Time)
waitLine (Blank cnt) = (True, Visible last last)
waitLine (Visible y x) = case prev y of
  Just y' => (False, Visible y' last)
  Nothing => (False, Blank last)

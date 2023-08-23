module Emu.HL2.Clock

import Data.DPair
import Data.Prim.Int
import Control.Monad.Identity
import Control.Monad.Writer
import Data.IORef

%default total

public export
CPUFreq : Int
CPUFreq = 4_000_000

public export
FPS : Int
FPS = 50

public export
HorizCount : Int
HorizCount = 256

public export
VisibleLines : Int
VisibleLines = 288

public export
FrameCount : Int
-- FrameCount = (CPUFreq `divBits32NZ` FPS) % search -- This blows up typechecking time :/
FrameCount = 80_000

public export
BlankCount : Int
BlankCount = FrameCount - (HorizCount * VisibleLines)

0 Counter : Int -> Type
Counter n = Subset Int (< n)

public export
0 Ticks : Type
Ticks = Counter FrameCount

public export
Show Ticks where
  show (Element i _) = show i

export
startTime : Ticks
startTime = Element 0 $ mkLT Refl

mod : {n : Int} -> Int -> Counter n
mod k = Element (k `mod` n) ?modLT

namespace Writers
  %hint
  m : Monoid Int
  m = Additive

  overrun : Int -> Writer Int Ticks
  overrun k = do
    tell $ k `div` FrameCount
    pure $ mod k

  public export
  tick : Int -> Ticks -> (Ticks, Int)
  tick n (Element k p) = runWriter $ do
    overrun $ k + n

  public export
  waitLine : Ticks -> (Ticks, Int)
  waitLine (Element i p) = runWriter $ do
    overrun $ ((i + HorizCount - 1) `mod` HorizCount) * HorizCount

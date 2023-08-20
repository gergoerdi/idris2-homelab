module HL2.Clock

import Data.So
import Data.DPair
import Data.Nat
import Control.Monad.Writer
import Data.IORef

%default total

public export
CPUFreq : Int
CPUFreq = 4_000_000

public export
0 FPS : Int
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
Counter n = Subset Int (\k => So (k < n))

public export
0 Ticks : Type
Ticks = Counter FrameCount

public export
Show Ticks where
  show (Element i _) = show i

-- public export
-- data Time : Type where
--   Visible : Counter VisibleLines -> Counter HorizCount -> Time
--   Blank   : Counter BlankCount                         -> Time
--
-- fromTicks : Ticks -> Time
-- fromTicks (Element i p) =
--   if i < (VisibleLines * HorizCount) then
--     let x = i `mod` HorizCount
--         y = i `div` HorizCount
--     in Visible (Element y ?p3) (Element x ?p4)
--   else Blank (Element (i - VisibleLines * HorizCount) ?p5)
--
-- toTicks : Time -> Ticks
-- toTicks (Visible (Element y py) (Element x px)) = Element (y * HorizCount + x) ?p1
-- toTicks (Blank (Element cnt p)) = Element (BlankCount + cnt) ?p2

export
startTime : Ticks
startTime = Element 0 Oh

mod : {n : Int} -> Int -> Counter n
mod k = Element (k `mod` n) ?modLT

public export
tick : Int -> Ticks -> (Bool, Ticks)
tick n (Element i p) = case choose ((i + n) < FrameCount) of
  Left lt => (False, Element (i + n) lt)
  Right ge => (True, mod (i + n))

public export
waitLine : Ticks -> (Bool, Ticks)
waitLine (Element i p) = (i' >= FrameCount, mod i)
  where
    i' = ((i + HorizCount - 1) `mod` HorizCount) * HorizCount

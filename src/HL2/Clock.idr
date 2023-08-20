module HL2.Clock

import Data.So
import Data.DPair
import Data.Nat
import Control.Monad.Writer
import Data.IORef

%default total

public export
0 CPUFreq : Bits32
CPUFreq = 4_000_000

public export
0 FPS : Bits32
FPS = 50

public export
HorizCount : Bits32
HorizCount = 256

public export
VisibleLines : Bits32
VisibleLines = 288

public export
FrameCount : Bits32
-- FrameCount = (CPUFreq `divBits32NZ` FPS) % search -- This blows up typechecking time :/
FrameCount = 80_000

public export
BlankCount : Bits32
BlankCount = FrameCount - (HorizCount * VisibleLines)

0 Counter : Bits32 -> Type
Counter n = Subset Bits32 (\k => So (k < n))

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

public export
tick : Bits32 -> Ticks -> (Bool, Ticks)
tick n (Element i p) =
  let same_frame = (i + n) < FrameCount in
  if same_frame then (False, Element (i + n) ?p)
  else (True, Element ((i + n) `mod` FrameCount) ?q)

public export
waitLine : Ticks -> (Bool, Ticks)
waitLine (Element i p) = (i' >= FrameCount, Element (i' `mod` FrameCount) ?r)
  where
    i' = ((i + HorizCount - 1) `mod` HorizCount) * HorizCount

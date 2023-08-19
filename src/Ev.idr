module Ev

import CPU
import HL2.Clock
import Web.MVC

%default total

public export
data Ev : Type where
  Tick : Bits32 -> Ev
  NewFrame : Ev
  Step : Ev
  VideoOff : Ev
  VideoOn : Ev

public export
data CPUEv : Type where
  Init : CPUEv
  Run : CPU -> Ev -> CPUEv

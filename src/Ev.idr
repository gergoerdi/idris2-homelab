module Ev

import CPU
import HL2.Clock
import Web.MVC

%default total

public export
data Ev : Type where
  Init : Ev
  Tick : CPU -> Bits32 -> Ev
  NewFrame : CPU -> Ev
  Step : CPU -> Ev

module Ev

import CPU
import HL2.Clock
import Web.MVC

%default total

public export
data Ev : Type where
  Init : Ev
  GotCPU : CPU -> Ev
  Tick : Nat -> Ev
  NewFrame : Ev
  Step : Ev

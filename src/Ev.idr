module Ev

import CPU
import Web.MVC

%default total

public export
data Ev : Type where
  Init : Ev
  GotCPU : CPU -> Ev
  RomSpy : Bits16 -> Ev
  Step : Ev

module Ev

import HL2.Machine
import CPU

%default total

public export
data Ev : Type where
  Tick : Int -> Ev
  NewFrame : Ev
  VideoOff : Ev
  VideoOn : Ev

public export
data MachineEv : (m : Type -> Type) -> (ev : Type) -> Type where
  Init : (m () -> m () -> Machine m) -> MachineEv m ev
  Run : CPU -> Machine m -> ev -> MachineEv m ev

public export
Functor (MachineEv m) where
  map f (Init partialMachine) = Init partialMachine
  map f (Run cpu machine ev) = Run cpu machine (f ev)

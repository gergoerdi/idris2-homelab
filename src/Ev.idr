module Ev

import Tape

%default total

public export
data Ev : Type where
  Init : Ev
  TapeEv : Tape.Ev -> Ev

module MemoryMap

import Data.So
import Data.Fin
import Data.Buffer.Index
import Data.ByteVect
import JS.Array

%default total

public export
0 Addr : Nat -> Nat -> Type
Addr from to = Index (S (to `minus` from))

inRange :
     (from : Nat)
  -> (to : Nat)
  -> (addr : Nat)
  -> (0 lower : from `LTE` addr)
  -> (0 upper : addr `LTE` to)
  -> Addr from to
inRange from to addr lower upper = Element (addr `minus` from) $ LTESucc $ minusLteMonotone upper

public export
record MemoryUnit (m : Type -> Type) (addr : Type) (a : Type) where
  constructor MkMemoryUnit
  read : addr -> m a
  write : addr -> a -> m ()

export
contramap : (addr' -> addr) -> MemoryUnit m addr a -> MemoryUnit m addr' a
contramap f = { read $= (. f), write $= (. f) }

public export
record MapEntry (m : Type -> Type) (a : Type) where
  constructor MkMapEntry
  from, to : Nat
  {auto 0 prf : So (to >= from)}
  unit : MemoryUnit m (Addr from to) a

export
memoryMap : List (MapEntry m a) -> MemoryUnit m () a -> MemoryUnit m Nat a
memoryMap units unit0 = MkMemoryUnit (\addr => read (find addr) ()) (\addr => write (find addr) ())
  where
    find : (addr : Nat) -> MemoryUnit m () a
    find addr = go units
      where
        go : List (MapEntry m a) -> MemoryUnit m () a
        go (MkMapEntry from to unit :: units) = case (from <= addr, addr <= to) of
          (True, True) =>
            let 0 lower = fromMaybe (assert_total $ idris_crash "p") $ from `maybeLTE` addr
                0 upper = fromMaybe (assert_total $ idris_crash "q") $ addr `maybeLTE` to
            in contramap (\() => inRange from to addr lower upper) unit
          _ => go units
        go [] = unit0

export
unconnected : Applicative m => a -> MemoryUnit m addr a
unconnected x = MkMemoryUnit{ read = \_ => pure x, write = \_, _ => pure () }

export
readOnly : Applicative m => (addr -> m a) -> MemoryUnit m addr a
readOnly rd = MkMemoryUnit
  { read = rd
  , write = \addr, val => pure ()
  }

export
-- rom : Applicative m => ByteVect n -> MemoryUnit m (Index n) Bits8
-- rom bs = readOnly $ \addr => pure $ index addr bs
rom : HasIO m => Array a -> MemoryUnit m (Index n) a
rom arr = readOnly $ \(Element i _) => fromMaybe (assert_total $ idris_crash ("read " <+> show i)) <$> readIO arr (cast i)

export
ram : HasIO m => Array a -> MemoryUnit m (Index n) a
ram arr = MkMemoryUnit
  { read = \(Element i _) => fromMaybe (assert_total $ idris_crash ("read " <+> show i)) <$> readIO arr (cast i)
  , write = \(Element i _), val => writeIO arr (cast i) val
  }

export
trigger : Applicative m => a -> m () -> MemoryUnit m addr a
trigger val action = MkMemoryUnit
  { read = \_ => pure val
  , write = \_, _ => action
  }

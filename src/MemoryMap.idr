module MemoryMap

import Data.Prim.Bits16
import Data.Prim.Ord
import public Data.DPair

-- import Data.ByteVect
import JS.Buffer
import Data.Array.Fast

%default total

public export
0 RawAddr : Type
RawAddr = Bits16

public export
0 Addr : RawAddr -> Type
Addr max = Subset RawAddr (<= max)

public export
0 AddrFromTo : RawAddr -> RawAddr -> Type
AddrFromTo from to = Addr (to - from)

inRange :
     (from : RawAddr)
  -> (to : RawAddr)
  -> (0 valid : from <= to)
  -> (addr : RawAddr)
  -> (0 lower : from <= addr)
  -> (0 upper : addr <= to)
  -> AddrFromTo (cast from) (cast to)
inRange from to valid addr lower upper = Element (addr - from) $ case upper of
  Left lt => Left ?p
  Right eq => Right $ rewrite eq in Refl

public export
record MemoryUnit (m : Type -> Type) (addr : Type) (a : Type) where
  constructor MkMemoryUnit
  read : addr -> m a
  write : addr -> a -> m ()

export
%inline
contramap : (addr' -> addr) -> MemoryUnit m addr a -> MemoryUnit m addr' a
contramap f = { read $= (. f), write $= (. f) }

public export
record MapEntry (m : Type -> Type) (a : Type) where
  constructor MkMapEntry
  from, to : RawAddr
  {auto 0 valid : from <= to}
  unit : MemoryUnit m (AddrFromTo (cast from) (cast to)) a

export
memoryMap : List (MapEntry m a) -> MemoryUnit m () a -> MemoryUnit m RawAddr a
memoryMap units unit0 = MkMemoryUnit (\addr => read (find addr) ()) (\addr => write (find addr) ())
  where
    find : (addr : RawAddr) -> MemoryUnit m () a
    find addr = go units
      where
        go : List (MapEntry m a) -> MemoryUnit m () a
        go (MkMapEntry from to {valid} unit :: units) = case (from `testGT` addr, addr `testGT` to) of
          (Right0 lower, Right0 upper) => contramap (\() => inRange from to valid addr lower upper) unit
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
rom : HasIO m => UInt8Array -> MemoryUnit m (Addr n) Bits8
rom arr = readOnly $ \(Element i _) => readArray arr (cast i)

export
ram : HasIO m => UInt8Array -> MemoryUnit m (Addr n) Bits8
ram arr = MkMemoryUnit
  { read = \(Element i _) => readArray arr (cast i)
  , write = \(Element i _), val => writeArray arr (cast i) val
  }

export
trigger : Applicative m => a -> m () -> MemoryUnit m addr a
trigger val action = MkMemoryUnit
  { read = \_ => pure val
  , write = \_, _ => action
  }

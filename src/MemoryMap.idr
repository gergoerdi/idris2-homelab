module MemoryMap

import Data.So
import Data.Fin
import public Data.DPair
import Data.ByteVect
import JS.Array

%default total

public export
0 RawAddr : Type
RawAddr = Bits16

public export
0 Addr : Nat -> Type
Addr n = Subset RawAddr (\i => cast i `LT` n)

public export
0 AddrFromTo : Nat -> Nat -> Type
AddrFromTo from to = Addr (S (to `minus` from))

0 minusRaw :
     (x : RawAddr)
  -> (y : RawAddr)
  -> (prf : cast x `LTE` cast y)
  -> cast (y - x) = cast y `minus` cast x

inRange :
     (from : RawAddr)
  -> (to : RawAddr)
  -> (addr : RawAddr)
  -> (0 lower : cast from `LTE` cast addr)
  -> (0 upper : cast addr `LTE` cast to)
  -> AddrFromTo (cast from) (cast to)
inRange from to addr lower upper = Element (addr - from) $ LTESucc $ rewrite (minusRaw from addr lower) in minusLteMonotone upper

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
  from, to : RawAddr
  {auto 0 prf : So (to >= from)}
  unit : MemoryUnit m (AddrFromTo (cast from) (cast to)) a

export
memoryMap : List (MapEntry m a) -> MemoryUnit m () a -> MemoryUnit m RawAddr a
memoryMap units unit0 = MkMemoryUnit (\addr => read (find addr) ()) (\addr => write (find addr) ())
  where
    find : (addr : RawAddr) -> MemoryUnit m () a
    find addr = go units
      where
        go : List (MapEntry m a) -> MemoryUnit m () a
        go (MkMapEntry from to unit :: units) = case (from <= addr, addr <= to) of
          (True, True) =>
            let 0 lower = fromMaybe (assert_total $ idris_crash "p") $ cast from `maybeLTE` cast addr
                0 upper = fromMaybe (assert_total $ idris_crash "q") $ cast addr `maybeLTE` cast to
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
rom : ArrayLike arr a => HasIO m => arr -> MemoryUnit m (Addr n) a
rom arr = readOnly $ \(Element i _) => fromMaybe (assert_total $ idris_crash ("read " <+> show i)) <$> readIO arr (cast i)

%foreign "javascript:lambda: (_arr, _a, xs, idx, val) => { xs[idx] = val }"
prim__writeIO : arr -> Bits32 -> a -> PrimIO ()

public export
writeIO : ArrayLike arr a => HasIO m => arr -> Bits32 -> a -> m ()
writeIO arr idx val = primIO $ prim__writeIO arr idx val

export
ram : ArrayLike arr a => HasIO m => arr -> MemoryUnit m (Addr n) a
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

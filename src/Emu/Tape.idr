module Emu.Tape

import JS.Array
import Data.DPair
import Web.Audio
import Emu.HL2.Clock

%default total

record AudioTape where
  constructor MkAudioTape
  sampleRate : Bits32
  buffer : IArray Double

public export
0 Time : Type
Time = Bits32

namespace AudioTape
  toTime : Bits32 -> Bits32 -> Time
  toTime sampleRate i = cast $ (the Bits64 (cast i) * cast CPUFreq) `div` cast sampleRate

  tapeLength : AudioTape -> Bits32
  tapeLength tape = toTime tape.sampleRate $ size tape.buffer


  toIndex : Bits32 -> Time -> Bits32
  toIndex sampleRate pos = cast $ (the Bits64 (cast pos) * cast sampleRate) `div` cast CPUFreq

  read : AudioTape -> Time -> Bool
  read tape pos =
    let i = min (size tape.buffer) $ toIndex tape.sampleRate pos
    in JS.Array.read tape.buffer (Element i ?minLT) > 0.03

export
data Tape : Type where
  Audio : AudioTape -> Tape

readTape : Tape -> Time -> Bool
readTape (Audio tape) = read tape

export
tapeLength : Tape -> Time
tapeLength (Audio tape) = tapeLength tape

export
audioTape : AudioBuffer -> IO Tape
audioTape buf = pure $ Audio $ MkAudioTape
  { sampleRate = sampleRate buf
  , buffer = !(freezeCloneIO $ channelData buf 0)
  }

public export
record Deck where
  constructor MkDeck
  tape : Maybe Tape
  position: Bits32
  playing : Bool
  recording : Bool

public export
startDeck : Deck
startDeck = MkDeck
  { tape = Nothing
  , position = 0
  , playing = False
  , recording = False
  }

export
tick : Int -> Deck -> Deck
tick n deck =
  let True = deck.playing | _ => deck
      Just tape = deck.tape | _ => deck
  in { position := min (tapeLength tape) $ deck.position + cast n } deck

export
read : Deck -> Bool
read deck =
  let True = deck.playing | _ => False
      Just tape = deck.tape | _ => False
  in readTape tape deck.position

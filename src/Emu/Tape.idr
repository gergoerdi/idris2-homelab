module Emu.Tape

import JS.Array
import Web.Audio
import Emu.HL2.Clock

record AudioTape where
  constructor MkAudioTape
  sampleRate : Bits32
  buffer : IArray Double

namespace AudioTape
  tapeLength : AudioTape -> Bits32
  tapeLength tape = (size tape.buffer * cast CPUFreq) `div` tape.sampleRate

data Tape : Type where
  Audio : AudioTape -> Tape

export
tapeLength : Tape -> Bits32
tapeLength (Audio tape) = tapeLength tape

audioTape : AudioBuffer -> Tape
-- audioTape buf = Audio $ MkAudioTape
--   { sampleRate = sampleRate buf
--   , buffer = freeze $ channelData buf 0
--   }

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
loadAudioTape : AudioBuffer -> Deck
loadAudioTape audioBuffer = { tape := Just $ audioTape audioBuffer } startDeck

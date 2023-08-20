module Tape

import Web.MVC
import Web.Html
import Web.MVC.Util
import Text.HTML.Ref
import JS.Array
import HL2.Clock

%default total

export
data Ev : Type where
  PlayPause : Ev
  Record : Bool -> Ev
  Rewind : Ev
  Eject : Ev

record AudioTape where
  constructor MkAudioTape
  sampleRate : Bits32
  buffer : IArray Double

namespace AudioTape
  tapeLength : AudioTape -> Bits32
  tapeLength tape = (size tape.buffer * cast CPUFreq) `div` tape.sampleRate

data Tape : Type where
  Audio : AudioTape -> Tape

tapeLength : Tape -> Bits32
tapeLength (Audio tape) = tapeLength tape

export
record St where
  constructor MkSt
  tape : Maybe Tape
  position: Bits32
  playing : Bool
  recording : Bool

public export
startTape : St
startTape = MkSt
  { tape = Nothing
  , position = 0
  , playing = False
  , recording = False
  }

playBtn : Ref Tag.Button
playBtn = Id "tape-btn-play"

rewindBtn : Ref Tag.Button
rewindBtn = Id "tape-btn-rewind"

recordBtn : Ref Tag.Input
recordBtn = Id "tape-btn-record"

ejectBtn : Ref Tag.Button
ejectBtn = Id "tape-btn-eject"

tracker : Ref Tag.Input
tracker = Id "tape-range"

fileSel : Ref Tag.Dialog
fileSel = Id "tape-filesel"

public export
update : Ev -> St -> St
update PlayPause = { playing $= not }
update Rewind = { position := 0 }
update (Record b) = { recording := b }
update Eject = id

public export
display : Maybe Ev -> St -> Cmd Ev
display ev s = batch
  [ disabled rewindBtn (isNothing s.tape)
  , disabled playBtn (isNothing s.tape)
  , disabled recordBtn (isNothing s.tape)

  , child playBtn $ span [ classes ["bi", if s.playing then "bi-pause-fill" else "bi-play-fill"] ] []
  , attr recordBtn $ checked s.recording
  , attr tracker $ showAttr "max" $ maybe 0 tapeLength s.tape
  , value tracker $ show s.position
  ] <+> case ev of
  Just Eject => cmd_ $ do
    dialog <- castElementByRef {t = HTMLDialogElement} fileSel
    showModal dialog
  _ => neutral

public export
setupEvents : St -> Cmd Ev
setupEvents s = batch
  [ display Nothing s
  , attr playBtn $ onClick $ PlayPause
  , attr recordBtn $ onChecked Record
  , attr rewindBtn $ onClick Rewind
  , attr ejectBtn $ onClick Eject
  ]

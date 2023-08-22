module Tape

import Web.MVC

export
data Ev : Type where
  Play   : Bool -> Ev
  Record : Bool -> Ev
  Rewind : Ev

-- export
-- record Tape where
--   constructor MkTape
--   position : Bits32
--   length :

export
record St where
  constructor MkSt
  tapePosition : Bits32
  playing : Bool
  recording : Bool

public export
startTape : St
startTape = MkSt
  { tapePosition = 0
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

positionRange : Ref Tag.Input
positionRange = Id "tape-range"

public export
update : Ev -> St -> St
update (Play b) = { playing := b }
update Rewind = { tapePosition := 0 }
update (Record b) = { recording := b }

public export
display : St -> Cmd Ev
display s = batch
  [ attr playBtn $ onClick $ Play (not s.playing)
  , child playBtn $ span [ classes ["bi", if s.playing then "bi-pause-fill" else "bi-play-fill"] ] []
  -- , attr recordBtn $ onChecked Record
  -- , attr recordBtn $ checked s.recording
  -- , attr rewindBtn $ onClick Rewind
  ]

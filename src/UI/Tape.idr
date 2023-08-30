module UI.Tape

import Data.IORef
import UI.Mutable

import Web.MVC
import Web.MVC.Util
import Web.MVC.Widget
import Web.Html

import Text.HTML.Ref

import Web.Audio
import Emu.Tape
import UI.Tape.Selector

%default total

public export
data Ev : Type where
  NewFrame : Ev
  PlayPause : Bool -> Ev
  Record : Bool -> Ev
  Rewind : Ev
  Eject : Ev

export
record St where
  constructor MkSt

public export
startTape : St
startTape = MkSt
  {
  }

playBtn : Ref Tag.Input
playBtn = Id "tape-btn-play"

rewindBtn : Ref Tag.Button
rewindBtn = Id "tape-btn-rewind"

recordBtn : Ref Tag.Input
recordBtn = Id "tape-btn-record"

ejectBtn : Ref Tag.Button
ejectBtn = Id "tape-btn-eject"

tracker : Ref Tag.Input
tracker = Id "tape-range"

monitor : Ref Tag.Div
monitor = Id "tape-monitor"

updateState : (St -> St) -> (St -> (St, Deck -> Deck))
updateState f s = (f s, id)

updateDeck : (Deck -> Deck) -> (St -> (St, Deck -> Deck))
updateDeck f s = (s, f)

update : Ev -> St -> (St, Deck -> Deck)
update (PlayPause b) = updateDeck { playing := b }
update (Record b) = updateDeck { recording := b }
update Rewind = updateDeck { position := 0, playing := False, recording := False }
update _ = updateState id

export
updateView : St -> Deck -> Cmd ev
updateView s deck = batch
  [ disabled rewindBtn (isNothing deck.tape)
  , disabled playBtn (isNothing deck.tape)
  , disabled recordBtn (isNothing deck.tape)

  -- , child playBtn $ span [ classes ["bi", if deck.playing then "bi-pause-fill" else "bi-play-fill"] ] []
  , checked recordBtn deck.recording
  , checked playBtn deck.playing
  , attr tracker $ showAttr "max" $ maybe 0 tapeLength deck.tape
  , value tracker $ show deck.position
  , attr monitor $ Str "data-state" $ if deck.recording then "write" else "read"
  , attr monitor $ Bool "data-value" $ read deck
  ]

export
setupView : St -> Deck -> Cmd Ev
setupView s deck = batch
  [ attr playBtn $ onChecked PlayPause
  , attr recordBtn $ onChecked Record
  , attr rewindBtn $ onClick Rewind
  , attr ejectBtn $ onClick Eject
  , updateView s deck
  ]

export
display : Ev -> St -> Deck -> Cmd Ev
display ev s deck = updateView s deck <+> case ev of
  PlayPause _ => cmd_ $ blur =<< castElementByRef {t = HTMLElement } playBtn
  Record _ => cmd_ $ blur =<< castElementByRef {t = HTMLElement } recordBtn
  Rewind => cmd_ $ blur =<< castElementByRef {t = HTMLElement } rewindBtn
  Eject => batch
    [ cmd_ $ blur =<< castElementByRef {t = HTMLElement } ejectBtn
    , cmd_ $ showModal =<< castElementByRef {t = HTMLDialogElement} UI.Tape.Selector.dialog
    ]
  _ => neutral

tapeWidget : (JSIO () -> JSIO ()) -> Mutable JSIO Deck -> Widget
tapeWidget registerFrameListener deck = addSetup w $ C $ \h => registerFrameListener $ h Tape.NewFrame
  where
    w = mutableWidget St Ev (MkSt{ }) setupView update display deck

public export
widget : (JSIO () -> JSIO ()) -> Mutable JSIO Deck -> Widget
widget registerFrameListener = tapeWidget registerFrameListener <+> Tape.Selector.widget

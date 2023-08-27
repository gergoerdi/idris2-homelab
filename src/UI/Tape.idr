module UI.Tape

import Data.IORef
import UI.Mutable

import JSON.Simple.Derive

import Web.MVC
import Web.MVC.Util
import Web.MVC.Http
import Web.MVC.Widget
import Web.Html

import Text.HTML.Ref
import JS.Array

import Paths
import Web.Audio
import Emu.Tape

%default total
%language ElabReflection

record TapeMeta where
  constructor MkTapeMeta
  filename : String
  title : String
  desc : Maybe String
  footer : Maybe String

%runElab derive "TapeMeta" [customFromJSON $ { replaceMissingKeysWithNull := True } defaultOptions]

%foreign "javascript:lambda: (url, cb) => load_audio_(url, cb)"
prim__loadAudio : String -> (AudioBuffer -> IO ()) -> PrimIO ()

public export
data Ev : Type where
  NewFrame : Ev
  PlayPause : Bool -> Ev
  Record : Bool -> Ev
  Rewind : Ev
  Eject : Ev
  LoadTapes : Either HTTPError (List TapeMeta) -> Ev
  LoadTape : String -> Ev
  TapeLoaded : Tape -> Ev

export
record St where
  constructor MkSt
  tapes : List TapeMeta

public export
startTape : St
startTape = MkSt
  { tapes = []
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

tapeselDlg : Ref Tag.Dialog
tapeselDlg = Id "tape-filesel"

monitor : Ref Tag.Div
monitor = Id "tape-monitor"

tapeList : Ref Tag.Div
tapeList = Id "tape-cards"

updateState : (St -> St) -> (St -> (St, Deck -> Deck))
updateState f s = (f s, id)

updateDeck : (Deck -> Deck) -> (St -> (St, Deck -> Deck))
updateDeck f s = (s, f)

update : Ev -> St -> (St, Deck -> Deck)
update (PlayPause b) = updateDeck { playing := b }
update (Record b) = updateDeck { recording := b }
update Rewind = updateDeck { position := 0, playing := False, recording := False }
update (TapeLoaded tape) = updateDeck { tape := Just tape, playing := False, recording := False }
update (LoadTapes err_tapes) = updateState $ case err_tapes of
  Left err => id
  Right tapes => { tapes := tapes }
update _ = updateState id

tapeCard : TapeMeta -> Node Ev
tapeCard tape = div [class "card"]
  [ div [class "card-body", onClick $ LoadTape (tapeFile tape.filename) ]
    [ h5 [class "card-title"] [Text tape.title]
    , p [class "card-text"] [Text $ fromMaybe "" tape.desc]
    ]
  , div [class "card-footer"]
    [ div [classes ["d-flex", "justify-content-between"]]
      [ span [class "text-body-secondary"] [Text $ fromMaybe "" tape.footer]
      , a [class "card-link", href (tapeFile tape.filename), download tape.filename ] [Text "Download"]
      ]
    ]
  ]

printError : HTTPError -> String
printError Timeout = "connection timed out"
printError NetworkError = "error when connecting to server"
printError (BadStatus m) = "server responded with bad status code: \{show m}"
printError (JSONError str x) =
  """
  Error when decoding JSON string: \{str}

  \{prettyErr str x}
  """

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
    [ getJSON (tapeFile "tapes.json") LoadTapes
    , cmd_ $ blur =<< castElementByRef {t = HTMLElement } ejectBtn
    , cmd_ $ showModal =<< castElementByRef {t = HTMLDialogElement} tapeselDlg
    ]
  LoadTapes (Left err) =>
    cmd_ $ putStrLn (printError err)
  LoadTapes (Right _) =>
    children tapeList $ map tapeCard s.tapes
  LoadTape filename => batch
    [ C $ \enqueueEvent => do
        close =<< castElementByRef {t = HTMLDialogElement} tapeselDlg
        liftIO $ primIO $ prim__loadAudio filename $ \audioBuffer => do
          tape <- audioTape audioBuffer
          runJS . enqueueEvent . TapeLoaded $ tape
     ]
  _ => neutral

public export
widget : Mutable JSIO Deck -> Widget
widget = mutableWidget St Ev (MkSt{ tapes = [] }) setupView update display

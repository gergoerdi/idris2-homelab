module UI.Tape

import JSON.Simple.Derive

import Web.MVC
import Web.MVC.Util
import Web.MVC.Http
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

export
data Ev : Type where
  PlayPause : Ev
  Record : Bool -> Ev
  Rewind : Ev
  Eject : Ev
  LoadTapes : Either HTTPError (List TapeMeta) -> Ev
  LoadTape : String -> Ev
  TapeLoaded : AudioBuffer -> Ev

export
record St where
  constructor MkSt
  tapes : List TapeMeta
  deck : Deck

updateDeck : (Deck -> Deck) -> St -> St
updateDeck f = { deck $= f }

public export
startTape : St
startTape = MkSt
  { tapes = []
  , deck = startDeck
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

tapeselDlg : Ref Tag.Dialog
tapeselDlg = Id "tape-filesel"

monitor : Ref Tag.Div
monitor = Id "tape-monitor"

tapeList : Ref Tag.Div
tapeList = Id "tape-cards"

public export
update : Ev -> St -> St
update PlayPause = updateDeck { playing $= not }
update Rewind = updateDeck { position := 0 }
update (Record b) = updateDeck { recording := b }
update Eject = id
update (LoadTapes err_tapes) = case err_tapes of
  Left err => id
  Right tapes => { tapes := tapes }
update (LoadTape filename) = id -- TODO
update (TapeLoaded audioBuffer) = { deck := loadAudioTape audioBuffer }

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

updateView : St -> Cmd Ev
updateView s = batch
  [ disabled rewindBtn (isNothing s.deck.tape)
  , disabled playBtn (isNothing s.deck.tape)
  , disabled recordBtn (isNothing s.deck.tape)

  , child playBtn $ span [ classes ["bi", if s.deck.playing then "bi-pause-fill" else "bi-play-fill"] ] []
  , attr recordBtn $ checked s.deck.recording
  , attr tracker $ showAttr "max" $ maybe 0 tapeLength s.deck.tape
  , value tracker $ show s.deck.position
  ]

public export
display : Ev -> St -> Cmd Ev
display ev s = updateView s <+> case ev of
  Eject => batch
    [ getJSON (tapeFile "tapes.json") LoadTapes
    , cmd_ $ showModal =<< castElementByRef {t = HTMLDialogElement} tapeselDlg
    ]
  LoadTapes (Left err) =>
    cmd_ $ putStrLn (printError err)
  LoadTapes (Right _) =>
    children tapeList $ map tapeCard s.tapes
  LoadTape filename => batch
    [ C $ \enqueueEvent => do
        close =<< castElementByRef {t = HTMLDialogElement} tapeselDlg
        liftIO $ primIO $ prim__loadAudio filename $ runJS . enqueueEvent . TapeLoaded
    ]
  _ => neutral

public export
setupEvents : St -> Cmd Ev
setupEvents s = batch
  [ updateView s
  , attr playBtn $ onClick $ PlayPause
  , attr recordBtn $ onChecked Record
  , attr rewindBtn $ onClick Rewind
  , attr ejectBtn $ onClick Eject
  ]

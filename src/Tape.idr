module Tape

import JSON.Simple.Derive

import Web.MVC
import Web.MVC.Util
import Web.MVC.Http
import Web.Html

import Text.HTML.Ref
import JS.Array

import Paths
import HL2.Clock

%default total
%language ElabReflection

record TapeMeta where
  constructor MkTapeMeta
  filename : String
  title : String
  desc : Maybe String
  footer : Maybe String

%runElab derive "TapeMeta" [customFromJSON $ { replaceMissingKeysWithNull := True } defaultOptions]

export
data Ev : Type where
  PlayPause : Ev
  Record : Bool -> Ev
  Rewind : Ev
  Eject : Ev
  LoadTapes : Either HTTPError (List TapeMeta) -> Ev
  LoadTape : String -> Ev

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
  tapes : List TapeMeta
  tape : Maybe Tape
  position: Bits32
  playing : Bool
  recording : Bool

public export
startTape : St
startTape = MkSt
  { tapes = []
  , tape = Nothing
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

tapeselDlg : Ref Tag.Dialog
tapeselDlg = Id "tape-filesel"

monitor : Ref Tag.Div
monitor = Id "tape-monitor"

tapeList : Ref Tag.Div
tapeList = Id "tape-cards"

public export
update : Ev -> St -> St
update PlayPause = { playing $= not }
update Rewind = { position := 0 }
update (Record b) = { recording := b }
update Eject = id
update (LoadTapes err_tapes) = case err_tapes of
  Left err => id
  Right tapes => { tapes := tapes }
update (LoadTape filename) = id -- TODO

tapeCard : TapeMeta -> Node Ev
tapeCard tape = div [class "card"]
  [ div [class "card-body", onClick $ LoadTape tape.filename ]
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
  Just Eject => batch
    [ getJSON (tapeFile "tapes.json") LoadTapes
    , cmd_ $ showModal =<< castElementByRef {t = HTMLDialogElement} tapeselDlg
    ]
  Just (LoadTapes (Left err)) =>
    cmd_ $ putStrLn (printError err)
  Just (LoadTapes (Right _)) =>
    children tapeList $ map tapeCard s.tapes
  Just (LoadTape filename) => batch
    [ cmd_ $ do
        close =<< castElementByRef {t = HTMLDialogElement} tapeselDlg
    ]
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

module UI.Tape.Selector

import UI.Mutable

import JSON.Simple.Derive

import Web.MVC
import Web.MVC.Util
import Web.MVC.Http
import Web.MVC.Widget
import Web.Html

import Text.HTML.Ref

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

data Ev : Type where
  LoadTapes : Either HTTPError (List TapeMeta) -> Ev
  LoadTape : String -> Ev
  TapeLoaded : Tape -> Ev
  NewTape : Ev

record St where
  constructor MkSt
  tapes : List TapeMeta

export
dialog : Ref Tag.Dialog
dialog = Id "tape-filesel"

tapeList : Ref Tag.Div
tapeList = Id "tape-cards"

updateState : (St -> St) -> (St -> (St, Deck -> Deck))
updateState f s = (f s, id)

updateDeck : (Deck -> Deck) -> (St -> (St, Deck -> Deck))
updateDeck f s = (s, f)

update : Ev -> St -> (St, Deck -> Deck)
update (TapeLoaded tape) = updateDeck { tape := Just tape, playing := False, recording := False }
update (LoadTapes err_tapes) = updateState $ case err_tapes of
  Left err => id
  Right tapes => { tapes := tapes }
update _ = updateState id

icon : String -> Node ev
icon s = span [classes ["bi", "bi-" <+> s]] []

tapeCard : ev -> List (Node ev) -> List (Node ev) -> List (Node ev) -> Node ev
tapeCard ev title body footer = div [class "card"] $
  [ div [class "card-body", onClick ev]
    [ h5 [class "card-title"] title
    , p [class "card-text"] body
    ]
  ] ++
  [ div [class "card-footer"] footer | not (null footer) ]

loadTapeCard : TapeMeta -> Node Ev
loadTapeCard tape = tapeCard (LoadTape (tapeFile tape.filename))
  [Text tape.title]
  [Text $ fromMaybe "" tape.desc]
  [ div [classes ["d-flex", "justify-content-between"]]
    [ span [class "text-body-secondary"] [Text $ fromMaybe "" tape.footer]
    , a [class "card-link", href (tapeFile tape.filename), download tape.filename ]
      [icon "file-earmark-arrow-down", Text "Download"]
    ]
  ]

addTapeCard : Node Ev
addTapeCard = tapeCard NewTape
  [icon "file-earmark-plus", Text "New tape"]
  []
  []

printError : HTTPError -> String
printError Timeout = "connection timed out"
printError NetworkError = "error when connecting to server"
printError (BadStatus m) = "server responded with bad status code: \{show m}"
printError (JSONError str x) =
  """
  Error when decoding JSON string: \{str}

  \{prettyErr str x}
  """

setupView : St -> Deck -> Cmd Ev
setupView s deck = getJSON (tapeFile "tapes.json") LoadTapes

display : Ev -> St -> Deck -> Cmd Ev
display ev s deck = case ev of
  LoadTapes (Left err) =>
    cmd_ $ putStrLn (printError err)
  LoadTapes (Right _) =>
    children tapeList $ map loadTapeCard s.tapes ++ [addTapeCard]
  LoadTape filename => batch
    [ C $ \enqueueEvent => do
        close =<< castElementByRef {t = HTMLDialogElement} dialog
        liftIO $ primIO $ prim__loadAudio filename $ \audioBuffer => do
          tape <- audioTape audioBuffer
          runJS . enqueueEvent . TapeLoaded $ tape
     ]
  _ => neutral

public export
widget : Mutable JSIO Deck -> Widget
widget = mutableWidget St Ev (MkSt{ tapes = [] }) setupView update display

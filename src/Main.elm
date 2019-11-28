module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Tafl.Game as Game
import Tafl.Variants as Variants
import Tafl.Web as Web

type Msg = GameMsg Game.Action | ViewMsg Web.Event
type alias Model = (Game.Game, Web.View)

init = (Game.begin Variants.fetlarBoard, Web.init)

update msg (game, view) =
    case msg of
        GameMsg gmsg -> (Game.update gmsg game, Web.gameUpdate gmsg view)
        ViewMsg vmsg ->
            case Web.viewUpdate vmsg view of
                (Just gmsg, view2) -> (Game.update gmsg game, view2)
                (Nothing, view2) -> (game, view2)

webview (game, view) =
    Web.render game view
    |> Html.map ViewMsg

main = Browser.sandbox
    { init = init
    , update = update
    , view = webview
    }


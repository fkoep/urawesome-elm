module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, h2, input, li, ul)
import Html.Events exposing (onClick)
import Html.Attributes exposing (type_, placeholder, class)
import Dict exposing (Dict)
import Dict as Dict
import WebUtil exposing (onEnter)
import Tafl.Game exposing (Game)
import Tafl.Game as Game
import Tafl.Variants as Variants
import Tafl.View exposing (ViewModel)
import Tafl.View as View

type Msg
    = CreateSession String
    | LoadSession String
    | DeleteSession String
    | GameAction String Game.Action
    | ViewEvent View.Event

type alias Model =
    { active: Maybe (String, ViewModel)
    , sessions: Dict String Game
    }

init =
    { active = Just ("mygame" , View.init)
    , sessions = Dict.singleton "mygame" <| Game.create Variants.fetlarBoard
    }

update msg model =
    case msg of
        CreateSession name ->
            { model
            | sessions = Dict.insert name (Game.create Variants.fetlarBoard) model.sessions
            , active = Just (name, View.init)
            }
        DeleteSession name ->
            { model | sessions = Dict.remove name model.sessions }
        LoadSession name ->
            { model | active = Just (name, View.init) }
        GameAction name act ->
            { model | sessions = Dict.update name (Maybe.map <| Game.update act) model.sessions }
        ViewEvent evt ->
            let updateview view2 = { model | active = Maybe.map (Tuple.mapSecond <| always view2) model.active } in
            model.active
            |> Maybe.map (\(name, view) -> 
                case View.eventUpdate evt view of
                    (Just act, view2) -> update (GameAction name act) (updateview view2)
                    (Nothing, view2) -> updateview view2)
            |> Maybe.withDefault model

viewGame: Model -> (String, ViewModel) -> Html Msg
viewGame model (name, view) =
    div 
        []
        [ h2 [] [text name]
        , Dict.get name model.sessions
        |> Maybe.map (\game -> Html.map ViewEvent <| View.view game view)
        |> Maybe.withDefault (div [] [])
        ]

viewSessionCreator: Html Msg
viewSessionCreator =
    div 
        []
        [input
            [type_ "input"
            , placeholder "session name"
            , onEnter CreateSession
            ]
            []
        ]

viewSessionEntry: (String, Game) -> Html Msg
viewSessionEntry (name, game) =
    li
        [onClick (LoadSession name)]
        [text name]

viewSessionList: List (String, Game) -> Html Msg
viewSessionList sessions =
    ul
        [class "session-list"]
        (List.map viewSessionEntry sessions)

webview model =
    div
        []
        [ model.active
        |> Maybe.map (viewGame model)
        |> Maybe.withDefault (div [] [])
        , viewSessionCreator
        , viewSessionList <| Dict.toList model.sessions
        ]
        
main = Browser.sandbox
    { init = init
    , update = update
    , view = webview
    }


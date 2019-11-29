module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, h2)
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Dict as Dict
import Tafl.Game exposing (Game)
import Tafl.Game as Game
import Tafl.Variants as Variants
import Tafl.View exposing (View)
import Tafl.View as View

type Msg
    = CreateSession String
    -- | LoadSession String
    | DeleteSession String
    | GameMsg String Game.Action
    | ViewMsg View.Event

type alias Model =
    { active: Maybe (String, View)
    , sessions: Dict String Game
    }

init =
    { active = Just ("mygame" , View.create)
    , sessions = Dict.singleton "mygame" <| Game.create Variants.fetlarBoard
    }

update msg model =
    case msg of
        CreateSession name ->
            { model
            | sessions = Dict.insert name (Game.create Variants.fetlarBoard) model.sessions
            , active = Just (name, View.create)
            }
        -- DeleteSession name ->
        --     { model | sessions = Dict.remove name model.sessions }
        GameMsg name gmsg ->
            { model
            | sessions = Dict.update name (Maybe.map <| Game.update gmsg) model.sessions
            }
        ViewMsg vmsg ->
            let updateview view2 = { model | active = Maybe.map (Tuple.mapSecond <| always view2) model.active } in
            model.active
            |> Maybe.map (\(name, view) -> 
                case View.viewUpdate vmsg view of
                    (Just act, view2) -> update (GameMsg name act) (updateview view2)
                    (Nothing, view2) -> updateview view2)
            |> Maybe.withDefault model
        _ -> model

renderView: Model -> (String, View) -> Html Msg
renderView model (name, view) =
    div 
        []
        [ h2 [] [text name]
        , Dict.get name model.sessions
        |> Maybe.map (\game -> Html.map ViewMsg <| View.render game view)
        |> Maybe.withDefault (div [] [])
        ]

webview model =
    model.active
    |> Maybe.map (renderView model)
    |> Maybe.withDefault (div [] [])
        
main = Browser.sandbox
    { init = init
    , update = update
    , view = webview
    }


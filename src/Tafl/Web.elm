module Tafl.Web exposing (..)

import Dict exposing (Dict)
import Dict as Dict
import Json.Encode as Encode
import Json.Decode as Decode
import Html exposing (Html, div)
import Html.Attributes exposing (attribute, class, draggable)
import Html.Events exposing (onClick)
import Html.Events as Events
import Coord exposing (Coord, Dir)
import Coord as Coord
import Board as Board
import Tafl.Game as Game

optList: List (a, Bool) -> List a
optList = List.filter (Tuple.second) >> List.map (Tuple.first)

-- TODO do we need this?
encodeCoord: Coord -> Encode.Value
encodeCoord (x, y) = Encode.list Encode.int [x, y]

-- TODO do we need this?
coordDecoder: Decode.Decoder Coord
coordDecoder = Decode.map2 Tuple.pair (Decode.index 0 Decode.int) (Decode.index 1 Decode.int)

-- TODO make drag-over in relation to CSS-:hover() work...
-- TODO how does Decode.succeed work here?
onDragStart msg = Events.on "dragstart" <| Decode.succeed msg
onDragEnd msg = Events.on "dragend" <| Decode.succeed msg
onDragOver msg = Events.preventDefaultOn "dragover" <| Decode.succeed (msg, True)
onDrop msg = Events.preventDefaultOn "drop" <| Decode.succeed (msg, True)

type Msg = SelectPiece Coord | UnselectPiece | TargetField Coord | HoverField

type alias Model = 
    { selected: Maybe Coord
    , moves: Dict Coord (List Coord)
    , state: Game.State
    }

init: Game.State -> Model
init s = { selected = Nothing, moves = Game.moves s, state = s }

update: Msg -> Model -> Model
update msg model =
    case msg of
        (SelectPiece c) -> { model | selected = Just c }
        UnselectPiece -> { model | selected = Nothing }
        (TargetField to) ->
            model.selected
            |> Maybe.map (\from ->
                let (ms, s) = Game.move from to model.state in 
                { selected = Nothing, moves = ms, state = s })
            |> Maybe.withDefault model
        HoverField -> model

pieceClass p =
    case p of
        Game.Attacker -> "attacker"
        Game.Defender -> "defender"
        Game.King -> "king"
        
tileClass t =
    case t of
        Game.Blank -> "blank"
        Game.Corner -> "corner"
        Game.Center -> "center"

canUnselectPiece model y x =
    model.selected == Just (x, y)

canSelectPiece model y x =
    not (canUnselectPiece model y x)
    && (model.moves
        |> Dict.get (x, y)
        |> Maybe.map (not << List.isEmpty)
        |> Maybe.withDefault False)

renderPiece model y x p =
    let sel = canSelectPiece model y x in
    let unsel = canUnselectPiece model y x in
    div
        (optList
            [ (class "piece", True)
            , (class (pieceClass p), True)
            , (class "selectable", sel)
            , (class "selected", unsel)
            , (onClick (SelectPiece (x, y)), sel)
            , (onClick UnselectPiece, unsel)
            , (draggable "true", sel || unsel)
            , (onDragStart (SelectPiece (x, y)), sel)
            ])
        []
        
canTargetField model y x =
    model.selected
    |> Maybe.map (\c -> Dict.get c model.moves) 
    |> Maybe.andThen identity
    |> Maybe.map (List.member (x, y))
    |> Maybe.withDefault False

renderField model y x (t, o) =
    let target = canTargetField model y x in
    div
        (optList 
            [ (class "field", True)
            , (class (tileClass t), True)
            , (class ("targetable"), target)
            , (onClick (TargetField (x, y)), target)
            , (onDragOver HoverField, target)
            , (onDrop (TargetField (x, y)), target)
            ])
        [ o
        |> Maybe.map (renderPiece model y x)
        |> Maybe.withDefault (div [] [])
        ]

renderRow model y row =
    div 
        [ class "row" ]
        <| List.indexedMap (renderField model y) row

renderBoard model b =
    div 
        [ class "board" ]
        <| List.indexedMap (renderRow model) (Board.toFields b)

view: Model -> Html Msg
view model = 
    div 
        [ class "game", class "tafl" ]
        [ renderBoard model model.state.board ]

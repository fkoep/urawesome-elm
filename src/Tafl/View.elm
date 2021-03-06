module Tafl.View exposing (..)

import Dict exposing (Dict)
import Dict as Dict
import Html exposing (Html, div, h2, text)
import Html.Attributes exposing (attribute, class, draggable)
import Html.Events exposing (onClick)
import Coord exposing (Coord, Dir)
import Coord as Coord
import Board as Board
import Tafl.Game exposing (Game, Action(..), Color(..), Piece(..), Tile(..), Field)
import Tafl.Game as Game
import WebUtil exposing (..)

type Event
    = MovePiece Coord Coord
    | SelectPiece Coord
    | UnselectPiece
    | TargetField Coord
    | HoverField -- TODO remove?

type alias View =
    { selected: Maybe Coord
    }

create: View
create = { selected = Nothing }

-- TODO naming?
-- TODO remove?
gameUpdate: Action -> View -> View
gameUpdate _ view = view

-- TODO naming?
viewUpdate: Event -> View -> (Maybe Action, View)
viewUpdate evt view =
    case evt of
        (MovePiece from to) -> (Just (Move from to), { view | selected = Nothing })
        (SelectPiece c) -> (Nothing, { view | selected = Just c })
        UnselectPiece -> (Nothing, { view | selected = Nothing })
        (TargetField to) -> (Maybe.map (\from -> Move from to) view.selected , view)
        HoverField -> (Nothing, view)

pieceClass p =
    case p of
        Attacker -> "attacker"
        Defender -> "defender"
        King -> "king"
        
tileClass t =
    case t of
        Blank -> "blank"
        Corner -> "corner"
        Center -> "center"

canUnselectPiece game view c =
    view.selected == Just c

canSelectPiece game view c =
    not (canUnselectPiece game view c)
    && (game.actions
        |> List.filter (\(Move from _) -> from == c)
        |> (not << List.isEmpty))

renderPiece: Game -> View -> Int -> Int -> Piece -> Html Event
renderPiece game view y x piece =
    let sel = canSelectPiece game view (x, y) in
    let unsel = canUnselectPiece game view (x, y) in
    div
        (optList
            [ (class "piece", True)
            , (class <| pieceClass piece, True)
            , (class "selectable", sel)
            , (class "selected", unsel)
            , (onClick <| SelectPiece (x, y), sel)
            , (onClick UnselectPiece, unsel)
            , (draggable "true", sel || unsel)
            , (onDragStart <| SelectPiece (x, y), sel)
            ])
        []

canTargetField game view to =
    view.selected
    |> Maybe.map (\from -> game.actions
        |> List.filter (\(Move from2 _) -> from == from2)
        |> List.map (\(Move _ to2) -> to2)
        |> List.member to)
    |> Maybe.withDefault False
        
renderField: Game -> View -> Int -> Int -> Field -> Html Event
renderField game view y x (tile, occ) =
    let target = canTargetField game view (x, y) in
    div
        (optList 
            [ (class "field", True)
            , (class <| tileClass tile, True)
            , (class "targetable", target)
            , (onClick <| TargetField (x, y), target)
            , (onDragOver HoverField, target)
            , (onDrop <| TargetField (x, y), target)
            ])
        (occ
            |> Maybe.map (List.singleton << renderPiece game view y x)
            |> Maybe.withDefault [])

        
renderRow: Game -> View -> Int -> List Field -> Html Event
renderRow game view y row =
    div 
        [ class "row" ]
        (List.indexedMap (renderField game view y) row)

renderBoard: Game -> View -> Html Event
renderBoard game view =
    div 
        [ class "board" ]
        (List.indexedMap (renderRow game view) (Board.toFields game.state.board))

colorName: Color -> String
colorName col =
    case col of
        Black -> "black"
        White -> "white"

renderTurn: Game -> View -> Html Event
renderTurn game view =
    h2 [] [text <| "Turn: " ++ colorName game.state.turn ]   

-- TODO naming?
render: Game -> View -> Html Event
render game view =
    div 
        [ class "game", class "tafl" ]
        [ renderBoard game view
        , renderTurn game view
        ]

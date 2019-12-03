-- TODO
-- module Tafl.Game exposing (Color, Piece, Tile, State, Action, Change, update)
module Tafl.Game exposing (..)

import Dict exposing (Dict)
import Dict as Dict -- TODO get rid of this?
import Coord exposing (Coord, Dir)
import Coord as Coord
import Board as Board

type Color = White | Black
type Piece = King | Defender | Attacker
type Tile = Center | Corner | Blank

type alias Field = Board.Field Tile Piece
type alias Board = Board.Board Tile Piece

type alias State =
    { turn: Color
    , board: Board
    }

init: Board -> State
init board = { turn = Black, board = board }

flipColor: Color -> Color
flipColor col =
    case col of
        White -> Black
        Black -> White

pieceOwner: Piece -> Color
pieceOwner piece =
    case piece of
        Attacker -> Black
        _ -> White

-- +++++ Capturing +++++

canPlace: Piece -> Tile -> Bool
canPlace piece tile =
    case (piece, tile) of
        (King, _) -> True
        (_, Blank) -> True
        _ -> False

fieldAssistsCapture: Piece -> Field -> State -> Bool
fieldAssistsCapture piece (tile, occ) st =
    Maybe.map pieceOwner occ == Just st.turn
    || occ == Nothing && not (canPlace piece tile) 

edgeAssistsKingCapture: State -> Bool
edgeAssistsKingCapture st =
    1 == List.length (Board.findPieces (\p -> pieceOwner p == White) st.board)

positionAssistsCapture: Piece -> Coord -> State -> Bool
positionAssistsCapture piece pos st =
    Board.field pos st.board
    |> Maybe.map (\f -> fieldAssistsCapture piece f st)
    |> Maybe.withDefault (piece == King && edgeAssistsKingCapture st)

assistDirs: Piece -> Dir -> List Dir
assistDirs piece cap =
    case piece of
       King -> [cap, Coord.perp cap, Coord.rperp cap]
       _ -> [cap]

testCapture: Dir -> Coord -> State -> Bool
testCapture cap pos st =
    -- TODO neater way of doing this?
    Board.piece pos st.board
    |> Maybe.andThen (\p -> if pieceOwner p /= st.turn then Just p else Nothing)
    |> Maybe.map (\piece ->
        assistDirs piece cap
        |> List.map (Coord.add pos)
        |> List.all (\apos -> positionAssistsCapture piece apos st))
    |> Maybe.withDefault False

captures: Coord -> State -> List Coord
captures pos st =
    Coord.directions
    |> List.filter (\dir -> testCapture dir (Coord.add pos dir) st)
    |> List.map (Coord.add pos)

-- +++++ Movement +++++

walkTargets: Piece -> Dir -> Coord -> State -> List Coord
walkTargets piece dir pos st =
    -- TODO neater way of doing this?
    case Board.field pos st.board of
        (Just (tile, Nothing)) ->
            if canPlace piece tile then
                pos :: walkTargets piece dir (Coord.add pos dir) st
            else if tile == Center then
                walkTargets piece dir (Coord.add pos dir) st
            else
                []
        _ -> []

targets: Coord -> Piece -> State -> List Coord
targets pos piece st =
    Coord.directions
    |> List.map (\dir -> walkTargets piece dir (Coord.add pos dir) st)
    |> List.concat 

moves: State -> List Action
moves st =
    Board.findPieces (\p -> pieceOwner p == st.turn) st.board
    |> List.map (\(pos, piece) -> targets pos piece st |> List.map (Move pos))
    |> List.concat

-- +++++ Win Conditions +++++

hasEscapePath: State -> Bool
hasEscapePath st = True -- TODO

hasEnded: State -> Bool
hasEnded st =
    List.head (Board.findPieces ((==) King) st.board)
    |> Maybe.map (\(pos, _) -> Board.tile pos st.board == Just Corner)
    -- |> Maybe.map (\v -> v || not (hasEscapePath s)) -- TODO neater way of doing this?
    |> Maybe.withDefault True

-- +++++ Update +++++

move: Action -> State -> (State, List Change, List Action)
move (Move from to) st =
    -- TODO neater way of doing this?
    let st2 = { st | board = Board.movePiece from to st.board } in
    let caps = captures to st2 in
    let st3 = { st2 | board = List.foldl Board.takePiece st2.board caps } in
    if hasEnded st3 then
        (st3, [], [])
    else
        let st4 = { st3 | turn = flipColor st3.turn } in
        (st4, [], moves st4)

-- +++++ Game +++++
-- TODO move up
type Action = Move Coord Coord
type Change = Moved Coord Coord | Removed Coord

-- TODO move outside
type alias Game =
    { state: State
    , changes: List Change
    , actions: List Action
    }

-- TODO naming?
create: Board -> Game
create b =
    let s = init b in
    { state = s, changes = [], actions = moves s }

update: Action -> Game -> Game
update act game =
    let (state, changes, actions) = move act game.state in
    { state = state, changes = changes, actions = actions }

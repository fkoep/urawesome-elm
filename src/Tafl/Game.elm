-- TODO
-- module Tafl.Game exposing (Color, Piece, Tile, State, Action, Change, update)
module Tafl.Game exposing (..)

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

flipColor: Color -> Color
flipColor col =
    case col of
        White -> Black
        Black -> White

pieceColor: Piece -> Color
pieceColor piece =
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
    Maybe.map pieceColor occ == Just st.turn
    || occ == Nothing && not (canPlace piece tile) 

edgeAssistsKingCapture: State -> Bool
edgeAssistsKingCapture st =
    1 == List.length (Board.findPieces (\p -> pieceColor p == White) st.board)

assistsCapture: Piece -> Coord -> State -> Bool
assistsCapture piece pos st =
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
    Board.piece pos st.board
    |> Maybe.map (\piece ->
        if pieceColor piece == flipColor st.turn then
            assistDirs piece cap
            |> List.map (Coord.add pos)
            |> List.all (\pos2 -> assistsCapture piece pos2 st)
        else
            False)
    |> Maybe.withDefault False

captures: Coord -> State -> List Coord
captures pos st =
    Coord.directions
    |> List.filter (\dir -> testCapture dir (Coord.add pos dir) st)
    |> List.map (Coord.add pos)

-- +++++ Movement +++++

walkTargets: Piece -> Dir -> Coord -> State -> List Coord
walkTargets piece dir pos st =
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

actions: State -> List Action
actions st =
    Board.findPieces (\p -> pieceColor p == st.turn) st.board
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
    let st2 = { st | board = Board.movePiece from to st.board } in
    let caps = captures to st2 in
    let st3 = { st2 | board = List.foldl Board.takePiece st2.board caps } in
    if hasEnded st3 then
        (st3, [], [])
    else
        let st4 = { st3 | turn = flipColor st3.turn } in
        (st4, [], actions st4)

-- +++++ Game +++++

type Action = Move Coord Coord
type Change = Moved Coord Coord | Removed Coord

type alias Game =
    { state: State
    , changes: List Change
    , actions: List Action
    }

create: Board -> Game
create board =
    let st = { turn = Black, board = board } in
    { state = st, changes = [], actions = actions st }

update: Action -> Game -> Game
update act game =
    let (st, chgs, acts) = move act game.state in
    { state = st, changes = chgs, actions = acts }

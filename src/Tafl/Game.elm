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
init b = { turn = Black, board = b }

flipColor: Color -> Color
flipColor c =
    case c of
        White -> Black
        Black -> White

pieceOwner: Piece -> Color
pieceOwner p =
    case p of
        Attacker -> Black
        _ -> White

-- +++++ Capturing +++++

canPlace: Piece -> Tile -> Bool
canPlace p t =
    case (p, t) of
        (King, _) -> True
        (_, Blank) -> True
        _ -> False

fieldAssistsCapture: Piece -> Field -> State -> Bool
fieldAssistsCapture p (t, o) s =
    Maybe.map pieceOwner o == Just s.turn
    || o == Nothing && not (canPlace p t) 

edgeAssistsKingCapture: State -> Bool
edgeAssistsKingCapture s =
    1 == List.length (Board.findPieces (\p -> pieceOwner p == White) s.board)

positionAssistsCapture: Piece -> Coord -> State -> Bool
positionAssistsCapture p c s =
    Board.field c s.board
    |> Maybe.map (\f -> fieldAssistsCapture p f s)
    |> Maybe.withDefault (p == King && edgeAssistsKingCapture s)

captureDirs: Dir -> Piece -> List Dir
captureDirs d p =
    case p of
       King -> [d, Coord.perp d, Coord.perp d]
       _ -> [d]

testCapture: Dir -> Coord -> State -> Bool
testCapture d c s =
    -- TODO neater way of doing this?
    Board.piece c s.board
    |> Maybe.andThen (\p -> if pieceOwner p /= s.turn then Just p else Nothing)
    |> Maybe.map (\p ->
        captureDirs d p
        |> List.map (Coord.add c)
        |> List.all (\cc -> positionAssistsCapture p cc s))
    |> Maybe.withDefault False

captures: Coord -> State -> List Coord
captures c s =
    Coord.directions
    |> List.filter (\d -> testCapture d (Coord.add c d) s)
    |> List.map (Coord.add c)

-- +++++ Movement +++++

walkTargets: Piece -> Dir -> Coord -> State -> List Coord
walkTargets p d c s =
    -- TODO neater way of doing this?
    case Board.field c s.board of
        (Just (t, Nothing)) ->
            if canPlace p t then
                c :: walkTargets p d (Coord.add c d) s
            else if t == Center then
                walkTargets p d (Coord.add c d) s
            else
                []
        _ -> []

targets: Coord -> Piece -> State -> List Coord
targets c p s =
    Coord.directions
    |> List.map (\d -> walkTargets p d (Coord.add c d) s)
    |> List.concat 

-- moves: State -> Dict Coord (List Coord)
-- moves s =
--     Board.findPieces (\p -> pieceOwner p == s.turn) s.board
--     -- TODO
--     -- Board.findPieces ((==) s.turn << pieceOwner) s.board
--     |> List.map (\(c, p) -> (c, targets c p s))
--     |> Dict.fromList

moves: State -> List Action
moves s =
    Board.findPieces (\p -> pieceOwner p == s.turn) s.board
    -- TODO
    -- Board.findPieces ((==) s.turn << pieceOwner) s.board
    |> List.map (\(c, p) -> targets c p s |> List.map (Move c))
    |> List.concat

-- +++++ Win Conditions +++++

hasEscapePath: State -> Bool
hasEscapePath s = True -- TODO

hasEnded: State -> Bool
hasEnded s =
    List.head (Board.findPieces ((==) King) s.board)
    |> Maybe.map (\(c, _) -> Board.tile c s.board == Just Corner)
    -- |> Maybe.map (\v -> v || not (hasEscapePath s)) -- TODO neater way of doing this?
    |> Maybe.withDefault True

-- +++++ Update +++++

move: Action -> State -> (State, List Change, List Action)
move (Move from to) s =
    -- TODO neater way of doing this?
    let s2 = { s | board = Board.movePiece from to s.board } in
    let caps = captures to s2 in
    let s3 = { s2 | board = List.foldl Board.takePiece s2.board caps } in
    if hasEnded s3 then
        (s3, [], [])
    else
        let s4 = { s3 | turn = flipColor s3.turn } in
        (s4, [], moves s4)

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

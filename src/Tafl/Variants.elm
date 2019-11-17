-- Rules here:
-- http://aagenielsen.dk/fetlar_rules_en.php
-- http://tafl.cyningstan.com/page/88/fetlar-hnefatafl
-- http://tafl.cyningstan.com/page/21/the-hnefatafl-board

module Tafl.Variants exposing (fetlarBoard)

import Dict as Dict -- TODO get rid of this?
import Coord exposing (Coord, Dir)
import Coord as Coord
import Board exposing (Board)
import Board as Board
import Tafl.Game exposing (..)

µ = (Blank, Nothing)
a = (Blank, Just Attacker)
k = (Center, Just King)
d = (Blank, Just Defender)
x = (Corner, Nothing)

-- TODO make use of this
mirrorQuarter q =
    let h = q ++ List.map (\r -> List.drop 1 (List.reverse r)) q in
    h ++ List.drop 1 (List.reverse h)

fetlarBoard = Board.fromFields
    [ [x, µ, µ, a, a, a, a, a, µ, µ, x]
    , [µ, µ, µ, µ, µ, a, µ, µ, µ, µ, µ]
    , [µ, µ, µ, µ, µ, µ, µ, µ, µ, µ, µ]
    , [a, µ, µ, µ, µ, d, µ, µ, µ, µ, a]
    , [a, µ, µ, µ, d, d, d, µ, µ, µ, a]
    , [a, a, µ, d, d, k, d, d, µ, a, a]
    , [a, µ, µ, µ, d, d, d, µ, µ, µ, a]
    , [a, µ, µ, µ, µ, d, µ, µ, µ, µ, a]
    , [µ, µ, µ, µ, µ, µ, µ, µ, µ, µ, µ]
    , [µ, µ, µ, µ, µ, a, µ, µ, µ, µ, µ]
    , [x, µ, µ, a, a, a, a, a, µ, µ, x]
    ] 
 
-- tablutBoard = Board.fromFields
 --    [ [X, µ, µ, A, A, A, µ, µ, X]
 --    , [µ, µ, µ, µ, A, µ, µ, µ, µ]
 --    , [µ, µ, µ, µ, D, µ, µ, µ, µ]
 --    , [A, µ, µ, µ, D, µ, µ, µ, A]
 --    , [A, A, D, D, K, D, D, A, A]
 --    , [A, µ, µ, µ, D, µ, µ, µ, A]
 --    , [µ, µ, µ, µ, D, µ, µ, µ, µ]
 --    , [µ, µ, µ, µ, A, µ, µ, µ, µ]
 --    , [X, µ, µ, A, A, A, µ, µ, X]
 --    ]

-- brandubBoard = Board.fromFields
 --    [ [X, µ, µ, A, µ, µ, X]
 --    , [µ, µ, µ, A, µ, µ, µ]
 --    , [µ, µ, µ, D, µ, µ, µ]
 --    , [A, A, D, K, D, A, A]
 --    , [µ, µ, µ, D, µ, µ, µ]
 --    , [µ, µ, µ, A, µ, µ, µ]
 --    , [X, µ, µ, A, µ, µ, X]
 --    ]


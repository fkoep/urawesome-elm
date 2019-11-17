module Board exposing (..)

import Coord exposing (Coord, Dir)
import Coord as Coord
import Dict exposing (Dict)
import Dict as Dict
import List.Extra

type alias Field tile piece = (tile, Maybe piece)
type alias Board tile piece = 
    { tiles: Dict Coord tile
    , pieces: Dict Coord piece
    }

dictsFromFields: List (List (Field tile piece)) -> (Dict Coord tile, Dict Coord piece)
dictsFromFields fs =
    Coord.flatten fs
    |> List.map (\(c, (t, o)) -> ((c, t), Maybe.andThen (Just << Tuple.pair c) o))
    |> List.unzip
    |> Tuple.mapSecond (List.filterMap identity)
    |> Tuple.mapBoth Dict.fromList Dict.fromList

fromFields: List (List (Field tile piece)) -> Board tile piece
fromFields fs = let (ts, ps) = dictsFromFields fs in { tiles = ts, pieces = ps }

toFields: Board tile piece -> (List (List (Field tile piece)))
toFields b =
    Dict.toList b.tiles
    |> List.map (\(c, t) -> (c, (t, (Dict.get c b.pieces))))
    |> Coord.unflatten

field: Coord -> Board tile piece -> Maybe (Field tile piece)
field c b = 
    Dict.get c b.tiles
    |> Maybe.map (\t -> (t, (Dict.get c b.pieces)))

piece: Coord -> Board tile piece -> Maybe piece
piece c b = Dict.get c b.pieces

tile: Coord -> Board tile piece -> Maybe tile
tile c b = Dict.get c b.tiles

takePiece: Coord -> Board tile piece -> Board tile piece
takePiece c b = { b | pieces = Dict.remove c b.pieces }

placePiece: Coord -> piece -> Board tile piece -> Board tile piece
placePiece c p b = { b | pieces = Dict.insert c p b.pieces }

movePiece: Coord -> Coord -> Board tile piece -> Board tile piece
movePiece from to b =
    Dict.get from b.pieces
    |> Maybe.map (\p -> placePiece to p (takePiece from b))
    |> Maybe.withDefault b

findPieces: (piece -> Bool) -> Board tile piece -> List (Coord, piece)
findPieces pred b =
    Dict.toList b.pieces
    |> List.filter (pred << Tuple.second)

-- walkDir: Dir -> Coord -> (Field tile piece -> Maybe r) -> Board tile piece -> List r
-- walkDir dir pos op b =
--     case getField (Coord.add pos dir) b of
--         (Just f) -> Maybe.default [] (Maybe.map list (op f))
        -- Nothing -> []

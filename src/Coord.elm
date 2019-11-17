module Coord exposing (..)

import List.Extra

type alias Coord = (Int, Int)
type alias Dir = Coord

add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2) 
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

directions = [north, east, south, west]

north = (0, 1)
east = (1, 0)
south = (0, -1)
west = (-1, 0)

perp (x, y) = (-y, x)
rperp = perp << perp << perp

flatten: List (List a) -> List (Coord, a)
flatten =
    List.indexedMap (\y -> List.indexedMap (\x -> Tuple.pair (x, y)))
    >> List.concat

flattenPartial: List (List (Maybe a)) -> List (Coord, a)
flattenPartial =
    List.indexedMap (\y -> List.indexedMap (\x -> Tuple.pair (x, y)))
    >> List.concat
    >> List.filterMap (\(c, v) -> Maybe.andThen (Just << Tuple.pair c) v)

unflatten: List (Coord, a) -> List (List a)
unflatten =
    List.Extra.gatherEqualsBy (\((_, y), _) -> y)
    >> List.map (\(g, r) -> g :: r)
    >> List.map (List.map Tuple.second)

-- TODO unflattenPartial

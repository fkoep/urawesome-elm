module WebUtil exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode
import Html.Events as Events
import Coord exposing (Coord)

optList: List (a, Bool) -> List a
optList = List.filter (Tuple.second) >> List.map (Tuple.first)

encodeCoord: Coord -> Encode.Value
encodeCoord (x, y) = Encode.list Encode.int [x, y]

coordDecoder: Decode.Decoder Coord
coordDecoder = Decode.map2 Tuple.pair (Decode.index 0 Decode.int) (Decode.index 1 Decode.int)

onDragStart msg = Events.on "dragstart" <| Decode.succeed msg
onDragEnd msg = Events.on "dragend" <| Decode.succeed msg
onDragOver msg = Events.preventDefaultOn "dragover" <| Decode.succeed (msg, True)
onDrop msg = Events.preventDefaultOn "drop" <| Decode.succeed (msg, True)


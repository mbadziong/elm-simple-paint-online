module Tuple exposing (tuple2Encoder, pointDecoder)

import Json.Encode exposing (list, Value)
import Json.Decode exposing (Decoder, tuple2, int)


tuple2Encoder : (a -> Value) -> (b -> Value) -> ( a, b ) -> Value
tuple2Encoder enc1 enc2 ( val1, val2 ) =
    list [ enc1 val1, enc2 val2 ]


pointDecoder : Decoder ( Int, Int )
pointDecoder =
    tuple2 (,) int int

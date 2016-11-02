module Line exposing (Line, encodedLine, linesDecoder)

import Color exposing (Color)
import ColorUtils exposing (colorToString, colorDecoder)
import Json.Encode exposing (list, string, object, encode, int)
import Json.Decode exposing (object2, (:=), Decoder, andThen)
import Tuple exposing (tuple2Encoder, pointDecoder)


type alias Point =
    ( Int, Int )


type alias Line =
    { points : List Point
    , lineColor : Color
    }


type alias EncodedLine =
    { points : List Point
    , lineColor : String
    }


encodedLine : Line -> String
encodedLine line =
    let
        lineColor =
            colorToString line.lineColor

        points =
            line.points

        entireMessage =
            object
                [ ( "lineColor", Json.Encode.string lineColor )
                , ( "points", Json.Encode.list (List.map (tuple2Encoder int int) points) )
                ]
                |> encode 0
    in
        entireMessage


lineDecoder : Decoder Line
lineDecoder =
    object2 Line
        ("points" := Json.Decode.list pointDecoder)
        ("lineColor" := colorDecoder)


linesDecoder : Decoder (List Line)
linesDecoder =
    Json.Decode.list lineDecoder

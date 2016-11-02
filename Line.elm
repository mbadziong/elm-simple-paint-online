module Line exposing (Line, encodedLine)

import Color exposing (Color)
import ColorUtils exposing (colorToString, stringToColor, decodedColors)
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



-- lineDecoder : Decoder Line
-- lineDecoder =
--     object2 Line
--         ("lineColor" := string `andThen` stringToColor)
--         ("points" := list pointDecoder)

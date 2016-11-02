module ColorUtils exposing (colorToString, colorDecoder)

import Json.Decode exposing (Decoder, decodeString, list, string, andThen, succeed, fail)
import Color exposing (Color, black, red, blue)


colorToString : Color -> String
colorToString col =
    if col == black then
        "black"
    else if col == red then
        "red"
    else if col == blue then
        "blue"
    else
        "black"


stringToColorDecoder : String -> Decoder Color
stringToColorDecoder str =
    if str == "black" then
        succeed black
    else if str == "red" then
        succeed red
    else if str == "blue" then
        succeed blue
    else
        fail "Error"


colorDecoder : Decoder Color
colorDecoder =
    Json.Decode.string `andThen` stringToColorDecoder

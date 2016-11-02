module ColorUtils exposing (colorToString, stringToColor, decodedColors)

import Json.Decode exposing (Decoder, decodeString, list, string)
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


stringToColor : String -> Color
stringToColor str =
    if str == "black" then
        black
    else if str == "red" then
        red
    else if str == "blue" then
        blue
    else
        black


decodedColors : String -> Result String (List String)
decodedColors colors =
    decodeString
        colorsDecoder
        colors


colorsDecoder : Decoder (List String)
colorsDecoder =
    Json.Decode.list Json.Decode.string

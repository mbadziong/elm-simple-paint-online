module Sketch exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Html exposing (Html, div, text, button, br)
import Html.Attributes exposing (..)
import Html.App as App
import Html.Events exposing (onClick)
import Mouse exposing (..)


type alias Model =
    { lines : List Line
    , currentLine : Line
    , isDrawing : Bool
    , x : Int
    , y : Int
    , windowWidth : Int
    , windowHeight : Int
    , lineColor : Color
    }


type alias Line =
    List Point


type alias Point =
    ( Int, Int )


type Msg
    = DrawStart Mouse.Position
    | DrawStop Mouse.Position
    | MouseMsg Mouse.Position
    | ChangeColor Color


initialModel : Model
initialModel =
    { lines = []
    , currentLine = []
    , isDrawing = False
    , x = 0
    , y = 0
    , windowWidth = 300
    , windowHeight = 300
    , lineColor = Color.red
    }


view : Model -> Html Msg
view model =
    let
        background =
            rect (toFloat model.windowWidth) (toFloat model.windowHeight)
                |> filled white

        createdLines =
            (drawLines model.lines model.lineColor)

        allElements =
            (List.append (background :: [ drawLine model.lineColor model.currentLine ]) createdLines)
    in
        div []
            [ div [ Html.Attributes.style [ ( "border-style", "solid" ), ( "display", "inline-block" ) ] ]
                [ collage
                    model.windowWidth
                    model.windowHeight
                    allElements
                    |> Element.toHtml
                ]
            , br [] []
            , button [ onClick (ChangeColor red) ] [ Html.text "red" ]
            , button [ onClick (ChangeColor blue) ] [ Html.text "blue" ]
            ]


drawLines : List Line -> Color -> List Form
drawLines lines lineColor =
    List.map (drawLine lineColor) lines


drawLine : Color -> List Point -> Form
drawLine lineColor points =
    let
        -- Our points are integers, but a path needs a list of floats.  We'll make a
        -- function to turn a 2-tuple of ints into a 2-tuple of floats
        intsToFloats : ( Int, Int ) -> ( Float, Float )
        intsToFloats ( x, y ) =
            ( toFloat x, toFloat y )

        -- Then we'll map our points across that function
        shape =
            path (List.map intsToFloats points)
    in
        -- Finally, we'll trace that list of points in solid red
        shape
            |> traced (solid lineColor)


main : Program Never
main =
    App.program
        { init = initialModel ! []
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.downs DrawStart
        , Mouse.moves MouseMsg
        , Mouse.ups DrawStop
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DrawStart _ ->
            ( { model | isDrawing = True, currentLine = [] }, Cmd.none )

        DrawStop _ ->
            ( saveLine model, Cmd.none )

        MouseMsg position ->
            ( isDrawing position model, Cmd.none )

        ChangeColor color ->
            ( { model | lineColor = color }, Cmd.none )


saveLine : Model -> Model
saveLine model =
    { model | y = 0, x = 0, lines = model.currentLine :: model.lines, isDrawing = False }


isDrawing : Mouse.Position -> Model -> Model
isDrawing position model =
    if model.isDrawing then
        mouse position model
    else
        model


mouse : Mouse.Position -> Model -> Model
mouse position model =
    { model | y = position.y, x = position.x, currentLine = ( position.x - (model.windowWidth // 2), (model.windowHeight // 2) - position.y ) :: model.currentLine }

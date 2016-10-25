module Sketch exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Html exposing (Html, div, text)
import Html.App as App
import Mouse exposing (..)


type alias Model =
    { points : List Point
    , x : Int
    , y : Int
    }


type alias Point =
    ( Int, Int )


type Msg
    = MouseMsg Mouse.Position


initialModel : Model
initialModel =
    { points = []
    , x = 0
    , y = 0
    }


view : Model -> Html Msg
view model =
    let
        ball =
            circle 10
                |> filled blue
                |> move ( toFloat (model.x - 150), toFloat (150 - model.y) )

        background =
            rect (toFloat 300) (toFloat 300)
                |> filled green
    in
        collage 300 300 [ background, ball ]
            |> Element.toHtml


drawLine : List Point -> Form
drawLine points =
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
            |> traced (solid red)


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
    Mouse.moves MouseMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMsg position ->
            ( mouse position model, Cmd.none )


mouse : Mouse.Position -> Model -> Model
mouse position model =
    { model | y = position.y, x = position.x, points = ( position.x, position.y ) :: model.points }

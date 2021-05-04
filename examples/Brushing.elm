module Brushing exposing (..)

{-| @category Advanced
-}

import Axis
import Browser
import Browser.Events
import Brush exposing (Brush, OnBrush, TwoDimensional)
import Color
import Events
import Json.Decode as D exposing (Decoder)
import Path
import Random
import Scale
import Shape
import Statistics
import Svg.Events exposing (custom)
import Time
import TypedSvg exposing (g, rect, svg)
import TypedSvg.Attributes exposing (cursor, fill, fillOpacity, opacity, pointerEvents, shapeRendering, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Attribute, Svg)
import TypedSvg.Types exposing (Cursor(..), Opacity(..), Paint(..), ShapeRendering(..), Transform(..))
import Zoom exposing (OnZoom, Zoom)


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Brush.subscriptions model.brush BrushMsg
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BrushMsg brushMsg ->
            ( { model | brush = Brush.update brushMsg model.brush }, Cmd.none )


view : Model -> Svg Msg
view model =
    svg [ viewBox 0 0 w h, width w, height h ]
        [ Brush.view [] BrushMsg model.brush
        ]


type alias Model =
    { brush : Brush TwoDimensional
    }


type Msg
    = BrushMsg OnBrush


init : () -> ( Model, Cmd Msg )
init () =
    ( { brush = Brush.initXY { n = padding, s = h - padding, w = padding, e = w - padding }
      }
    , Cmd.none
    )

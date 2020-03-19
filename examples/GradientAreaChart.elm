module GradientAreaChart exposing (main)

{-| This module shows how to build a simple line and area chart using some of
the primitives provided in this library.
-}

import Axis
import Color
import Dict
import Path exposing (Path)
import SampleData exposing (norwegianCarSales)
import Scale exposing (ContinuousScale)
import Shape
import Time
import TypedSvg exposing (defs, g, linearGradient, stop, svg)
import TypedSvg.Attributes exposing (class, fill, id, offset, stopColor, stroke, transform, viewBox, x1, x2, y1, y2)
import TypedSvg.Attributes.InPx exposing (strokeWidth)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Length(..), Paint(..), Transform(..))


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


xScale : ContinuousScale Float
xScale =
    Scale.linear ( 0, w - 2 * padding ) ( 0, 39 )


yScale : ContinuousScale Float
yScale =
    Scale.linear ( h - 2 * padding, 0 ) ( 0, 800 )


xAxis : List ( Float, Float ) -> Svg msg
xAxis model =
    Axis.bottom [ Axis.tickCount 10 ] xScale


yAxis : Svg msg
yAxis =
    Axis.left [ Axis.tickCount 8 ] yScale


tranfromToAreaData : ( Float, Float ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
tranfromToAreaData ( x, y ) =
    Just
        ( ( Scale.convert xScale x, Tuple.first (Scale.rangeExtent yScale) )
        , ( Scale.convert xScale x, Scale.convert yScale y )
        )


area : List ( Float, Float ) -> Path
area model =
    List.map tranfromToAreaData model
        |> Shape.area Shape.linearCurve


gradientDefs : List (Svg msg)
gradientDefs =
    [ linearGradient
        [ id "gradientDuo"
        , x1 <| Percent 0.0
        , y1 <| Percent 0.0
        , x2 <| Percent 0.0
        , y2 <| Percent 100.0
        ]
        [ stop [ offset "0%", stopColor "#FFCC80" ] []
        , stop [ offset "100%", stopColor "#F57C00" ] []
        ]
    ]


view : List ( Float, Float ) -> Svg msg
view model =
    svg [ viewBox 0 0 w h ]
        [ defs [] gradientDefs
        , g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis model ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ]
            [ Path.element (area model) [ strokeWidth 3, fill <| Reference "gradientDuo" ]
            ]
        ]



-- From here onwards this is simply example boilerplate.
-- In a real app you would load the data from a server and parse it, perhaps in
-- a separate module.


data : List ( Float, Float )
data =
    [ ( 0, 391 ), ( 1, 387 ), ( 2, 337 ), ( 3, 290 ), ( 4, 326 ), ( 5, 378 ), ( 6, 329 ), ( 7, 379 ), ( 8, 375 ), ( 9, 418 ), ( 10, 441 ), ( 11, 393 ), ( 12, 431 ), ( 13, 491 ), ( 14, 486 ), ( 15, 481 ), ( 16, 519 ), ( 17, 469 ), ( 18, 459 ), ( 19, 438 ), ( 20, 447 ), ( 21, 537 ), ( 22, 507 ), ( 23, 454 ), ( 24, 474 ), ( 25, 461 ), ( 26, 466 ), ( 27, 491 ), ( 28, 520 ), ( 29, 505 ), ( 30, 456 ), ( 31, 428 ), ( 32, 454 ), ( 33, 505 ), ( 34, 475 ), ( 35, 495 ), ( 36, 560 ), ( 37, 556 ), ( 38, 608 ), ( 39, 662 ) ]


main =
    view <| data

module Scatterplot exposing (main)

{-| This module shows how to build a very basic scatterplot.

@category Basics

-}

import Axis
import List
import Scale exposing (ContinuousScale)
import TypedSvg exposing (circle, defs, g, linearGradient, stop, svg)
import TypedSvg.Attributes exposing (class, fill, id, offset, opacity, stopColor, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Length(..), Opacity(..), Paint(..), Transform(..))


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
    Scale.linear ( 0, w - 2 * padding ) ( 0, 4.5 )


yScale : ContinuousScale Float
yScale =
    Scale.linear ( h - 2 * padding, 0 ) ( 0, 6 )


xAxis : Svg msg
xAxis =
    Axis.bottom [] xScale


yAxis : Svg msg
yAxis =
    Axis.left [ Axis.tickCount 5 ] yScale


circles : List ( Float, Float ) -> List (Svg msg)
circles model =
    List.map pointCircle model


pointCircle : ( Float, Float ) -> Svg msg
pointCircle ( dataX, dataY ) =
    g [ class [ "data-point" ] ]
        [ circle
            [ cx (Scale.convert xScale dataX)
            , cy (Scale.convert yScale dataY)
            , r 3
            , fill <| Reference "linGradientRed"
            , strokeWidth 0
            , stroke <| PaintNone
            , opacity <| Opacity 0.85
            ]
            []
        ]


myDefs : List (Svg msg)
myDefs =
    [ linearGradient
        [ id "linGradientRed"
        , TypedSvg.Attributes.x1 <| Percent 0.0
        , TypedSvg.Attributes.y1 <| Percent 0.0
        , TypedSvg.Attributes.x2 <| Percent 0.0
        , TypedSvg.Attributes.y2 <| Percent 100.0
        ]
        [ stop [ offset "0%", stopColor "#e52d27" ] []
        , stop [ offset "100%", stopColor "#b31217" ] []
        ]
    , linearGradient
        [ id "linGradientGray"
        , TypedSvg.Attributes.x1 <| Percent 0.0
        , TypedSvg.Attributes.y1 <| Percent 0.0
        , TypedSvg.Attributes.x2 <| Percent 0.0
        , TypedSvg.Attributes.y2 <| Percent 100.0
        ]
        [ stop [ offset "0%", stopColor "#5b6467" ] []
        , stop [ offset "74%", stopColor "#8b939a" ] []
        ]
    ]


view : List ( Float, Float ) -> Svg msg
view model =
    svg [ viewBox 0 0 w h ]
        --
        [ defs [] myDefs
        , g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ]
            (circles model)
        ]


main : Svg msg
main =
    view dataPoints


dataPoints : List ( Float, Float )
dataPoints =
    [ ( 2.07, 3.82 ), ( 2.48, 4.52 ), ( 3.82, 5.25 ), ( 1.6, 3.31 ), ( 1.0, 1.8 ), ( 1.35, 2.6 ), ( 2.41, 4.31 ), ( 2.64, 4.5 ), ( 2.22, 3.79 ), ( 3.29, 4.69 ), ( 1.03, 2.29 ), ( 3.19, 5.08 ), ( 3.81, 5.5 ), ( 2.83, 4.62 ), ( 3.72, 5.11 ), ( 3.29, 5.02 ), ( 0.67, 1.07 ), ( 3.96, 5.62 ), ( 2.66, 4.69 ), ( 2.18, 3.89 ), ( 1.64, 3.32 ), ( 2.19, 3.66 ), ( 2.12, 3.84 ), ( 0.56, 0.76 ), ( 1.22, 2.37 ), ( 1.5, 2.98 ), ( 3.36, 4.78 ), ( 0.99, 2.14 ), ( 2.88, 4.48 ), ( 0.7, 1.19 ), ( 0.6, 0.58 ), ( 1.74, 3.41 ), ( 2.43, 4.33 ), ( 2.29, 4.2 ), ( 3.66, 5.11 ), ( 3.54, 4.87 ), ( 3.58, 4.96 ), ( 2.54, 4.28 ), ( 3.04, 4.43 ), ( 0.64, 1.0 ), ( 3.91, 5.51 ), ( 1.66, 3.16 ), ( 3.52, 5.08 ), ( 2.24, 3.78 ), ( 3.33, 5.17 ), ( 2.0, 3.63 ), ( 1.42, 2.88 ), ( 3.14, 4.64 ), ( 3.54, 4.92 ), ( 1.36, 2.64 ), ( 2.15, 4.06 ), ( 3.21, 5.13 ), ( 2.55, 4.27 ), ( 3.02, 4.74 ), ( 3.09, 4.69 ), ( 2.59, 4.4 ), ( 0.61, 0.67 ), ( 3.01, 4.74 ), ( 3.83, 5.4 ), ( 0.99, 2.23 ), ( 0.59, 0.65 ), ( 2.62, 4.24 ), ( 0.89, 1.29 ), ( 2.19, 4.01 ), ( 4.0, 5.61 ), ( 2.37, 4.28 ), ( 0.98, 1.99 ), ( 1.11, 2.2 ), ( 3.96, 5.29 ), ( 1.94, 3.54 ), ( 2.44, 4.29 ), ( 2.9, 4.76 ), ( 2.46, 4.74 ), ( 2.37, 4.18 ), ( 2.31, 4.08 ), ( 1.36, 2.63 ), ( 0.8, 1.23 ), ( 2.53, 4.32 ), ( 0.98, 1.88 ), ( 2.64, 4.27 ), ( 2.41, 4.25 ), ( 0.64, 0.94 ), ( 3.93, 5.41 ), ( 1.55, 2.67 ), ( 3.26, 4.71 ), ( 1.06, 2.05 ), ( 2.78, 4.6 ), ( 2.39, 4.08 ), ( 0.78, 1.57 ), ( 3.7, 5.26 ), ( 1.01, 1.79 ), ( 2.65, 4.54 ), ( 2.82, 4.7 ), ( 1.07, 2.24 ), ( 2.64, 4.29 ), ( 2.09, 4.02 ), ( 1.64, 2.98 ), ( 3.19, 4.81 ), ( 2.33, 4.25 ), ( 0.97, 1.88 ), ( 3.0, 4.55 ), ( 1.56, 3.26 ), ( 3.01, 4.84 ), ( 2.12, 3.9 ), ( 2.71, 4.29 ), ( 3.88, 4.9 ), ( 2.79, 4.54 ), ( 2.86, 4.62 ), ( 3.57, 5.27 ), ( 2.37, 4.15 ), ( 1.13, 2.38 ), ( 1.38, 2.6 ), ( 1.09, 2.28 ), ( 3.39, 4.65 ), ( 1.08, 2.27 ), ( 1.83, 3.3 ), ( 3.21, 4.72 ), ( 2.77, 4.49 ), ( 3.45, 5.36 ), ( 0.76, 1.63 ), ( 2.55, 4.49 ), ( 3.97, 5.19 ), ( 3.54, 5.17 ), ( 2.31, 4.24 ), ( 3.79, 5.32 ), ( 0.6, 0.8 ), ( 2.66, 4.12 ), ( 1.19, 2.77 ), ( 1.67, 3.23 ), ( 1.69, 3.55 ), ( 0.83, 1.49 ), ( 0.56, 0.82 ), ( 2.62, 4.32 ), ( 3.17, 5.2 ), ( 1.04, 1.92 ), ( 3.37, 4.97 ), ( 2.36, 4.01 ), ( 3.24, 4.89 ), ( 1.04, 2.49 ), ( 2.84, 4.71 ), ( 2.23, 3.95 ), ( 2.14, 3.87 ), ( 2.47, 4.32 ), ( 3.99, 5.37 ), ( 3.88, 5.3 ), ( 2.81, 4.57 ), ( 1.43, 2.82 ), ( 2.0, 3.82 ), ( 3.08, 4.99 ), ( 1.77, 3.41 ), ( 3.63, 5.22 ), ( 2.23, 4.03 ), ( 0.97, 1.94 ), ( 2.49, 4.33 ), ( 1.11, 2.62 ), ( 1.23, 2.64 ), ( 3.42, 4.77 ), ( 1.26, 2.36 ), ( 2.3, 4.32 ), ( 2.13, 3.72 ), ( 1.34, 2.56 ), ( 2.43, 4.34 ), ( 1.62, 3.3 ), ( 0.74, 1.34 ), ( 0.64, 0.87 ), ( 2.3, 3.97 ), ( 3.55, 5.15 ), ( 3.76, 5.28 ), ( 0.97, 2.0 ), ( 2.65, 4.23 ), ( 1.3, 2.89 ), ( 2.56, 4.11 ), ( 2.67, 4.51 ), ( 2.31, 3.69 ), ( 2.12, 3.87 ), ( 1.05, 2.16 ), ( 2.43, 4.29 ), ( 1.95, 4.0 ), ( 1.27, 2.45 ), ( 1.81, 3.36 ), ( 1.6, 3.26 ), ( 2.54, 4.5 ), ( 0.64, 0.84 ), ( 2.79, 4.68 ), ( 1.41, 3.31 ), ( 2.3, 4.2 ), ( 3.35, 5.15 ), ( 2.37, 4.12 ), ( 3.6, 5.05 ), ( 3.07, 4.51 ), ( 2.11, 4.02 ), ( 2.84, 4.73 ), ( 0.88, 1.94 ), ( 1.33, 2.91 ), ( 3.18, 5.11 ), ( 0.72, 0.97 ), ( 0.59, 0.9 ), ( 1.27, 2.68 ), ( 0.76, 1.66 ), ( 2.09, 3.66 ) ]

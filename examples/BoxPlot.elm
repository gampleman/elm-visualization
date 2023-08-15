module BoxPlot exposing (main)

{-| This module shows how to build a [box-and-whisker plot](https://en.wikipedia.org/wiki/Box_plot).

The trick is first computing all the statistical pieces that a box plot requires, then we use scales
to transform these into coordinates and finally draw the various SVG pieces.

@category Basics

-}

import Axis
import Color
import List.Extra
import LowLevel.Command exposing (lineTo, moveTo)
import Path
import Scale exposing (BandScale, ContinuousScale, defaultBandConfig)
import Statistics exposing (quantile)
import SubPath
import TypedSvg exposing (circle, defs, g, line, linearGradient, rect, stop, svg)
import TypedSvg.Attributes exposing (class, fill, id, offset, opacity, stopColor, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, strokeWidth, width, x, x1, x2, y, y1, y2)
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


xScale : List (List Float) -> BandScale Int
xScale model =
    List.range 0 (List.length model - 1)
        |> Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } ( 0, w - 2 * padding )


yScale : ContinuousScale Float
yScale =
    Scale.linear ( h - 2 * padding, 0 ) ( -12, 12 )


type alias BoxStats =
    { firstQuartile : Float, median : Float, thirdQuartile : Float, max : Float, min : Float, outliers : List Float }


computeStatistics : List Float -> BoxStats
computeStatistics yList =
    let
        sortedYList =
            List.sort yList

        reverseSortedYList =
            List.reverse sortedYList

        -- Gather stats
        firstQuartile =
            Statistics.quantile 0.25 sortedYList
                |> Maybe.withDefault 0

        thirdQuartile =
            Maybe.withDefault 0 <| quantile 0.75 sortedYList

        interQuartileRange =
            thirdQuartile - firstQuartile

        whiskerTopMax =
            thirdQuartile + 1.5 * interQuartileRange

        whiskerBottomMin =
            firstQuartile - (1.5 * interQuartileRange)
    in
    { firstQuartile = firstQuartile
    , median = Statistics.quantile 0.5 sortedYList |> Maybe.withDefault 0
    , thirdQuartile = thirdQuartile
    , max = computeWhiskerMax (<=) (Maybe.withDefault 0 (List.head reverseSortedYList)) whiskerTopMax reverseSortedYList
    , min = computeWhiskerMax (>=) (Maybe.withDefault 0 (List.head sortedYList)) whiskerBottomMin sortedYList
    , outliers =
        List.Extra.takeWhile (\y -> y < whiskerBottomMin) sortedYList
            ++ List.Extra.takeWhile (\y -> y > whiskerTopMax) reverseSortedYList
    }


{-| The whiskers should be either 1.5 the interquantile range or the highest datum, whichever is lowest.
-}
computeWhiskerMax : (number -> number -> Bool) -> number -> number -> List number -> number
computeWhiskerMax cmp dataMax whiskerMax sortedData =
    if cmp whiskerMax dataMax then
        Maybe.withDefault 0 <| List.head <| List.Extra.dropWhile (cmp whiskerMax) sortedData

    else
        dataMax


xAxis : List (List Float) -> Svg msg
xAxis model =
    Axis.bottom [] (Scale.toRenderable (\c -> c + 1 |> String.fromInt) (xScale model))


yAxis : Svg msg
yAxis =
    Axis.left [ Axis.tickCount 9 ] yScale


whisker : { max : Float, min : Float, width : Float, center : Float } -> Svg msg
whisker { max, min, width, center } =
    Path.element
        [ SubPath.with (moveTo ( center, min )) [ lineTo [ ( center, max ) ] ]
        , SubPath.with (moveTo ( center - width / 2, max )) [ lineTo [ ( center + width / 2, max ) ] ]
        ]
        [ strokeWidth 1
        , stroke <| Paint <| Color.black
        , opacity <| Opacity 0.7
        ]


column : BandScale Int -> BoxStats -> Int -> Svg msg
column scale stats index =
    let
        -- Prepare for viz
        seriesMedianY =
            stats.median
                |> Scale.convert yScale

        firstQuartileY =
            Scale.convert yScale stats.firstQuartile

        thirdQuartileY =
            Scale.convert yScale stats.thirdQuartile

        leftSide =
            Scale.convert scale index

        boxWidth =
            Scale.bandwidth scale

        center =
            leftSide + boxWidth / 2
    in
    g [ class [ "column" ] ]
        ([ whisker
            { max = Scale.convert yScale stats.max
            , min = thirdQuartileY
            , width = boxWidth / 3
            , center = center
            }
         , whisker
            { max = Scale.convert yScale stats.min
            , min = firstQuartileY
            , width = boxWidth / 3
            , center = center
            }
         , rect
            [ x leftSide
            , y thirdQuartileY
            , width boxWidth
            , height (firstQuartileY - thirdQuartileY)
            , fill <| Reference "linearGradient"
            , opacity <| Opacity 0.9
            ]
            []
         , line
            -- median line in middle of rectangle
            [ x1 leftSide
            , y1 seriesMedianY
            , x2 <| leftSide + boxWidth
            , y2 seriesMedianY
            , strokeWidth 1
            , stroke <| Paint <| Color.black
            , opacity <| Opacity 0.6
            ]
            []
         ]
            ++ List.map (Scale.convert yScale >> outlierCircle center) stats.outliers
        )


outlierCircle : Float -> Float -> Svg msg
outlierCircle x y =
    circle
        [ cx x
        , cy y
        , r 3
        , fill <| Paint <| Color.rgb255 180 20 20
        , stroke <| PaintNone
        , opacity <| Opacity 0.7
        ]
        []


yGridLine : Int -> Float -> Svg msg
yGridLine index tick =
    line
        [ x1 0
        , x2 (w - 2 * padding)
        , y1 (Scale.convert yScale tick)
        , y2 (Scale.convert yScale tick)
        , stroke <| Paint Color.black
        , strokeWidth (toFloat (modBy 2 index) * 0.25 + 0.25)
        , opacity <| Opacity 0.3
        ]
        []


gradient : Svg msg
gradient =
    linearGradient
        [ id "linearGradient"
        , TypedSvg.Attributes.x1 <| Percent 0.0
        , TypedSvg.Attributes.y1 <| Percent 0.0
        , TypedSvg.Attributes.x2 <| Percent 0.0
        , TypedSvg.Attributes.y2 <| Percent 100.0
        ]
        [ stop [ offset "0%", stopColor "#e52d27" ] []
        , stop [ offset "100%", stopColor "#b31217" ] []
        ]


view : List (List Float) -> Svg msg
view model =
    svg [ viewBox 0 0 w h ]
        [ defs [] [ gradient ]
        , g [ transform [ Translate padding (padding + 0.5) ] ] <| List.indexedMap yGridLine <| Scale.ticks yScale 9
        , g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis model ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ] <|
            List.indexedMap (\index datum -> column (xScale model) (computeStatistics datum) index) model
        ]


main : Svg msg
main =
    view dataSeries


dataSeries : List (List Float)
dataSeries =
    [ [ 2.0, 5.9, -3.6, 1.0, 5.4, -1.6, -9.2, -8.6, 5.0, 3.1, 9.7, -3.1, 7.0, 1.2, -4.1, -2.1, 3.6, 1.2, 1.5, -3.8, 1.0, 3.9, 8.3, -1.1, 1.7, -9.3, -7.6, -1.3, 2.5, 7.3, 9.0, 1.1, -1.3, -7.6, 1.3, 4.2, -4.5, 1.0, 7.7, -1.6 ]
    , [ 2.1, 2.7, 2.4, 2.3, 2.5, 1.5, 1.9, 2.1, 1.9, 2.0, 1.8, 1.1, 1.9, 2.4, 1.2, 2.5, 1.5, 2.3, 1.9, 3.0, 2.1, 1.8, 2.5, 1.1, 2.6, 2.2, 1.8, 2.2, 2.3, 2.2, 1.8, 2.3, 1.7, 1.8, 2.1, 2.1, 2.2, 1.4, 2.7, 1.7 ]
    , [ 4.9, 3.4, 3.1, 2.1, 2.7, -6.1, 3.5, 2.4, -1.2, -3.4, 3.1, 4.0, 4.8, 2.2, 9.4, 4.8, 9.5, 3.3, 1.7, -1.6, 1.9, 1.7, 2.5, 3.0, 2.4, 4.5, 1.2, 4.3, 1.2, 4.6, -1.4, -6.9, 2.0, 1.6, 3.2, 4.2, 8.5, 1.0, 3.2, 3.3 ]
    , [ 2.2, 7.8, -1.4, -1.5, -1.2, 9.5, 3.4, -1.6, 1.1, 1.4, 6.2, -8.8, -7.0, 3.6, -6.7, -4.9, -1.1, 6.7, -1.0, 1.4, -1.0, -5.7, -1.0, 7.6, -7.5, 1.2, -8.6, -7.3, -1.6, -7.2, -9.5, -5.3, 2.4, -5.5, -6.5, -1.5, 3.6, 3.1, -1.3, 5.7 ]
    , [ 9.1, 4.8, -1.1, 6.6, 6.5, 6.5, 1.2, 5.0, 9.9, 1.7, 7.3, 1.5, 1.4, 3.8, 9.4, 7.4, 8.4, 1.4, 1.4, 1.2, 1.0, 1.3, 2.0, 1.1, 1.4, 1.6, 1.4, 1.1, 3.7, 9.5, 6.7, 1.1, 7.5, 8.3, 6.8, 6.2, 2.3, 1.0, 9.3, 1.1 ]
    ]

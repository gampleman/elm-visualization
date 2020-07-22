module BoxPlot exposing (main)

{-| This module shows how to build a box-and-whisker plot, see <https://en.wikipedia.org/wiki/Box_plot> for details.
-}

import Axis
import Color
import List.Extra
import Scale exposing (BandScale, ContinuousScale, defaultBandConfig)
import Statistics exposing (median, quantile)
import TypedSvg exposing (circle, defs, g, line, linearGradient, rect, stop, svg)
import TypedSvg.Attributes exposing (class, fill, id, offset, opacity, stopColor, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, strokeWidth, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Opacity(..), Paint(..), Transform(..))


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


xAxisWidth : Float
xAxisWidth =
    w - 10 * padding


yAxisHeight : Float
yAxisHeight =
    h - 2 * padding


xScale : List (List Float) -> BandScale Int
xScale model =
    List.range 0 (List.length model - 1)
        |> Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } ( 0, xAxisWidth )


yScale : ContinuousScale Float
yScale =
    Scale.linear ( yAxisHeight, 0 ) ( -12, 12 )


xAxis : List (List Float) -> Svg msg
xAxis model =
    Axis.bottom [] (Scale.toRenderable (\c -> c + 1 |> String.fromInt) (xScale model))


yAxis : Svg msg
yAxis =
    Axis.left [ Axis.tickCount 9 ] yScale


column : BandScale Int -> List Float -> Int -> Svg msg
column scale yList indx =
    let
        orZero =
            Maybe.withDefault 0

        yConvert =
            Scale.convert yScale

        sortedYS =
            List.sort yList

        revSortYS =
            List.reverse sortedYS

        serieMedian =
            orZero <| median yList

        serieMedianY =
            yConvert serieMedian

        firstQuantile =
            orZero <| quantile 0.25 sortedYS

        firstQuantileY =
            yConvert firstQuantile

        thirdQuantile =
            orZero <| quantile 0.75 sortedYS

        thirdQuantileY =
            yConvert thirdQuantile

        interQuantileRange =
            thirdQuantile - firstQuantile

        maxY =
            orZero <| List.Extra.last sortedYS

        minY =
            orZero <| List.head sortedYS

        whiskerTopMax =
            thirdQuantile + 1.5 * interQuantileRange

        whiskerBottomMin =
            firstQuantile - (1.5 * interQuantileRange)

        whiskerTop =
            if maxY > whiskerTopMax then
                orZero <| List.head <| List.Extra.dropWhile (\y -> y >= whiskerTopMax) revSortYS

            else
                orZero <| List.head revSortYS

        whiskerBottom =
            if minY < whiskerBottomMin then
                orZero <| List.head <| List.Extra.dropWhile (\y -> y <= whiskerBottomMin) sortedYS

            else
                orZero <| List.head sortedYS

        whiskerTopY =
            yConvert whiskerTop

        whiskerBottomY =
            yConvert whiskerBottom

        indxXLeft =
            Scale.convert scale indx

        boxWidth =
            Scale.bandwidth scale

        whiskerWidth =
            boxWidth / 3

        boxMidX =
            indxXLeft + boxWidth / 2

        outliers =
            List.Extra.takeWhile (\y -> y < whiskerBottomMin) sortedYS
                ++ List.Extra.takeWhile (\y -> y > whiskerTopMax) revSortYS

        outliersY =
            List.map (\y -> yConvert y) outliers

        outliersXY =
            List.map (\y -> ( boxMidX, y )) outliersY
    in
    g [ class [ "column" ] ]
        ([ line
            --top vertical line through rectangle
            [ x1 boxMidX
            , y1 thirdQuantileY
            , x2 boxMidX
            , y2 whiskerTopY
            , strokeWidth 1
            , stroke <| Paint <| Color.black
            , opacity <| Opacity 0.7
            ]
            []
         , line
            --bottom vertical line through rectangle
            [ x1 boxMidX
            , y1 whiskerBottomY
            , x2 boxMidX
            , y2 firstQuantileY
            , strokeWidth 1
            , stroke <| Paint <| Color.black
            , opacity <| Opacity 0.7
            ]
            []
         , line
            --bottom whsiker
            [ x1 <| boxMidX - whiskerWidth / 2
            , y1 whiskerBottomY
            , x2 <| boxMidX + whiskerWidth / 2
            , y2 whiskerBottomY
            , strokeWidth 1
            , stroke <| Paint <| Color.black
            ]
            []
         , line
            --top whisker
            [ x1 <| boxMidX - whiskerWidth / 2
            , y1 whiskerTopY
            , x2 <| boxMidX + whiskerWidth / 2
            , y2 whiskerTopY
            , strokeWidth 1
            , stroke <| Paint <| Color.black
            ]
            []
         , rect
            [ x indxXLeft
            , y thirdQuantileY
            , width boxWidth
            , height (firstQuantileY - thirdQuantileY)
            , fill <| Reference "linGradientDuo"
            , opacity <| Opacity 0.8
            ]
            []
         , line
            -- median line in middle of rectangle
            [ x1 indxXLeft
            , y1 serieMedianY
            , x2 <| indxXLeft + boxWidth
            , y2 serieMedianY
            , strokeWidth 1
            , stroke <| Paint <| Color.black
            , opacity <| Opacity 0.6
            ]
            []
         ]
            ++ List.map outlierCircle outliersXY
        )


outlierCircle : ( Float, Float ) -> Svg msg
outlierCircle ( x, y ) =
    circle
        [ cx x
        , cy y
        , r 3
        , fill <| Paint <| Color.rgb255 180 20 20
        , strokeWidth 0
        , stroke <| PaintNone
        , opacity <| Opacity 0.7
        ]
        []


yGridLine : Int -> Float -> Svg msg
yGridLine index tick =
    line
        [ x1 0
        , x2 xAxisWidth
        , y1 (Scale.convert yScale tick)
        , y2 (Scale.convert yScale tick)
        , stroke <| Paint Color.black
        , strokeWidth (Basics.max (toFloat (modBy 2 index)) 0.5)
        , opacity <| Opacity 0.4
        ]
        []



-- rectangle for colored background
-- bgRect : Svg msg
-- bgRect =
--     rect [ x 0
--             , y 0
--             , width <| xAxisWidth
--             , height <| yAxisHeight
--             , fill <| Paint <| Color.red
--             , opacity <| Opacity 0.2
--             ]
--             []


myDefs : List (Svg msg)
myDefs =
    [ linearGradient
        [ id "linGradientDuo"
        , TypedSvg.Attributes.x1 <| Percent 0.0
        , TypedSvg.Attributes.y1 <| Percent 0.0
        , TypedSvg.Attributes.x2 <| Percent 0.0
        , TypedSvg.Attributes.y2 <| Percent 100.0
        ]
        [ stop [ offset "0%", stopColor "#e52d27" ] []
        , stop [ offset "100%", stopColor "#b31217" ] []
        ]
    ]


view : List (List Float) -> Svg msg
view model =
    svg [ viewBox 0 0 w h ]
        [ defs [] myDefs

        -- enable for colored plot background
        -- , g [ transform [ Translate padding padding ], class [ "bg-rect" ] ] <|
        --     [ bgRect ]
        , g [ transform [ Translate padding (padding + 0.5) ] ] <| List.indexedMap yGridLine <| Scale.ticks yScale 9
        , g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis model ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ] <|
            List.indexedMap (\indx lst -> column (xScale model) lst indx) model
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

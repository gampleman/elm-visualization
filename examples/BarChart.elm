module BarChart exposing (main)

{-| This module shows how to build a simple bar chart.

1.  It starts by setting out the basic variables to define a drawing area.
2.  Then it creates scales that encode the x and y dimensions of the bars.
3.  Based on these it sets up axes.
4.  Finally it assembles everything into a chart.

@category Basics

-}

import Axis
import DateFormat
import Scale exposing (BandScale, ContinuousScale, defaultBandConfig)
import Time
import TypedSvg exposing (g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Transform(..))


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


xScale : List ( Time.Posix, Float ) -> BandScale Time.Posix
xScale model =
    List.map Tuple.first model
        |> Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } ( 0, w - 2 * padding )


yScale : ContinuousScale Float
yScale =
    Scale.linear ( h - 2 * padding, 0 ) ( 0, 5 )


dateFormat : Time.Posix -> String
dateFormat =
    DateFormat.format [ DateFormat.dayOfMonthFixed, DateFormat.text " ", DateFormat.monthNameAbbreviated ] Time.utc


xAxis : List ( Time.Posix, Float ) -> Svg msg
xAxis model =
    Axis.bottom [] (Scale.toRenderable dateFormat (xScale model))


yAxis : Svg msg
yAxis =
    Axis.left [ Axis.tickCount 5 ] yScale


column : BandScale Time.Posix -> ( Time.Posix, Float ) -> Svg msg
column scale ( date, value ) =
    g [ class [ "column" ] ]
        [ rect
            [ x <| Scale.convert scale date
            , y <| Scale.convert yScale value
            , width <| Scale.bandwidth scale
            , height <| h - Scale.convert yScale value - 2 * padding
            ]
            []
        , text_
            [ x <| Scale.convert (Scale.toRenderable dateFormat scale) date
            , y <| Scale.convert yScale value - 5
            , textAnchor AnchorMiddle
            ]
            [ text <| String.fromFloat value ]
        ]


view : List ( Time.Posix, Float ) -> Svg msg
view model =
    svg [ viewBox 0 0 w h ]
        [ style [] [ text """
            .column rect { fill: rgba(118, 214, 78, 0.8); }
            .column text { display: none; }
            .column:hover rect { fill: rgb(118, 214, 78); }
            .column:hover text { display: inline; }
          """ ]
        , g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis model ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ] <|
            List.map (column (xScale model)) model
        ]


main : Svg msg
main =
    view timeSeries


timeSeries : List ( Time.Posix, Float )
timeSeries =
    [ ( Time.millisToPosix 1448928000000, 2.5 )
    , ( Time.millisToPosix 1451606400000, 2 )
    , ( Time.millisToPosix 1452211200000, 3.5 )
    , ( Time.millisToPosix 1452816000000, 2 )
    , ( Time.millisToPosix 1453420800000, 3 )
    , ( Time.millisToPosix 1454284800000, 1 )
    , ( Time.millisToPosix 1456790400000, 1.2 )
    ]

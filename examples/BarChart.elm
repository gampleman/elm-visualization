module BarChart exposing (main)

{-| This module shows how to build a simple bar chart.
-}

import DateFormat
import SampleData exposing (timeSeries)
import Time
import TypedSvg exposing (g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, textAnchor, transform)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Transform(..))
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)


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
    Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } (List.map Tuple.first model) ( 0, w - 2 * padding )


yScale : ContinuousScale
yScale =
    Scale.linear ( 0, 5 ) ( h - 2 * padding, 0 )


dateFormat : Time.Posix -> String
dateFormat =
    DateFormat.format [ DateFormat.dayOfMonthFixed, DateFormat.text " ", DateFormat.monthNameAbbreviated ] Time.utc


xAxis : List ( Time.Posix, Float ) -> Svg msg
xAxis model =
    Axis.axis { defaultOptions | orientation = Axis.Bottom } (Scale.toRenderable dateFormat (xScale model))


yAxis : Svg msg
yAxis =
    Axis.axis { defaultOptions | orientation = Axis.Left, tickCount = 5 } yScale


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
    svg [ width w, height h ]
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


main =
    view timeSeries

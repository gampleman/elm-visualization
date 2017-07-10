module BarChart exposing (main)

{-| This module shows how to build a simple bar chart.
-}

import Date exposing (Date)
import Date.Extra as Date
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import SampleData exposing (timeSeries)


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


xScale : List ( Date, Float ) -> BandScale Date
xScale model =
    Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } (List.map Tuple.first model) ( 0, w - 2 * padding )


yScale : ContinuousScale
yScale =
    Scale.linear ( 0, 5 ) ( h - 2 * padding, 0 )


xAxis : List ( Date, Float ) -> Svg msg
xAxis model =
    Axis.axis { defaultOptions | orientation = Axis.Bottom, tickFormat = Just (Date.toFormattedString "dd MMM") } (Scale.toRenderable (xScale model))


yAxis : Svg msg
yAxis =
    Axis.axis { defaultOptions | orientation = Axis.Left, tickCount = 5 } yScale


column : BandScale Date -> ( Date, Float ) -> Svg msg
column xScale ( date, value ) =
    g [ class "column" ]
        [ rect
            [ x <| toString <| Scale.convert xScale date
            , y <| toString <| Scale.convert yScale value
            , width <| toString <| Scale.bandwidth xScale
            , height <| toString <| h - Scale.convert yScale value - 2 * padding
            ]
            []
        , text_
            [ x <| toString <| Scale.convert (Scale.toRenderable xScale) date
            , y <| toString <| Scale.convert yScale value - 5
            , textAnchor "middle"
            ]
            [ text <| toString value ]
        ]


view : List ( Date, Float ) -> Svg msg
view model =
    svg [ width (toString w ++ "px"), height (toString h ++ "px") ]
        [ Svg.style [] [ text """
            .column rect { fill: rgba(118, 214, 78, 0.8); }
            .column text { display: none; }
            .column:hover rect { fill: rgb(118, 214, 78); }
            .column:hover text { display: inline; }
          """ ]
        , g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString (h - padding) ++ ")") ]
            [ xAxis model ]
        , g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString padding ++ ")") ]
            [ yAxis ]
        , g [ transform ("translate(" ++ toString padding ++ ", " ++ toString padding ++ ")"), class "series" ] <|
            List.map (column (xScale model)) model
        ]


main =
    view timeSeries

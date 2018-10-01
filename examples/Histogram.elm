module Histogram exposing (main)

{-| Renders a histogram of a randomly generated data set
-}

import Color
import Random exposing (Generator, Seed)
import TypedSvg exposing (g, rect, svg)
import TypedSvg.Attributes exposing (class, fill, transform)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Transform(..))
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Histogram as Histogram exposing (Bin, HistogramGenerator)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)


{-| We use addition here to approximate normal distribution.
-}
generator : Generator (List Float)
generator =
    Random.list 500 <| Random.map2 (+) (Random.float 0 10) (Random.float 0 10)


seed : Seed
seed =
    -- chosen by fair dice roll
    Random.initialSeed 227852860


data : List Float
data =
    Tuple.first <| Random.step generator seed


histogram : List Float -> List (Bin Float Float)
histogram model =
    Histogram.float
        |> Histogram.withDomain ( 0, 20 )
        |> Histogram.compute model


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


xScale : ContinuousScale
xScale =
    Scale.linear ( 0, 20 ) ( 0, w - 2 * padding )


yScaleFromBins : List (Bin Float Float) -> ContinuousScale
yScaleFromBins bins =
    List.map .length bins
        |> List.maximum
        |> Maybe.withDefault 0
        |> toFloat
        |> (\b -> ( 0, b ))
        |> (\a -> Scale.linear a ( h - 2 * padding, 0 ))


xAxis : List Float -> Svg msg
xAxis model =
    Axis.axis { defaultOptions | orientation = Axis.Bottom } xScale


yAxis : List (Bin Float Float) -> Svg msg
yAxis bins =
    Axis.axis { defaultOptions | orientation = Axis.Left, tickCount = 5 } (yScaleFromBins bins)


column : ContinuousScale -> Bin Float Float -> Svg msg
column yScale { length, x0, x1 } =
    rect
        [ x <| Scale.convert xScale x0
        , y <| Scale.convert yScale (toFloat length)
        , width <| Scale.convert xScale x1 - Scale.convert xScale x0
        , height <| h - Scale.convert yScale (toFloat length) - 2 * padding
        , fill <| Fill <| Color.rgb255 46 118 149
        ]
        []


view : List Float -> Svg msg
view model =
    let
        bins =
            histogram model
    in
    svg [ width w, height h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis model ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis bins ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ] <|
            List.map (column (yScaleFromBins bins)) bins
        ]


main =
    view data

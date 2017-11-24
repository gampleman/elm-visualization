module Histogram exposing (main)

{-| Renders a histogram of a randomly generated data set
-}

import Random.Pcg as Random exposing (Generator, Seed)
import Visualization.Histogram as Histogram exposing (Bin, HistogramGenerator)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)


{-| We use addition here to approximate normal distribution.
-}
generator : Generator (List Float)
generator =
    Random.list 500 <| Random.map2 (+) (Random.float 0 10) (Random.float 0 10)


seed : Seed
seed =
    Random.initialSeed 227852860


model : List Float
model =
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


yScale : List (Bin Float Float) -> ContinuousScale
yScale bins =
    List.map .length bins
        |> List.maximum
        |> Maybe.withDefault 0
        |> toFloat
        |> (,) 0
        |> flip Scale.linear ( h - 2 * padding, 0 )


xAxis : List Float -> Svg msg
xAxis model =
    Axis.axis { defaultOptions | orientation = Axis.Bottom } xScale


yAxis : List (Bin Float Float) -> Svg msg
yAxis bins =
    Axis.axis { defaultOptions | orientation = Axis.Left, tickCount = 5 } (yScale bins)


column : ContinuousScale -> ContinuousScale -> Bin Float Float -> Svg msg
column xScale yScale { length, x0, x1 } =
    rect
        [ x <| toString <| Scale.convert xScale x0
        , y <| toString <| Scale.convert yScale (toFloat length)
        , width <| toString <| Scale.convert xScale x1 - Scale.convert xScale x0
        , height <| toString <| h - Scale.convert yScale (toFloat <| Debug.log "length" length) - 2 * padding
        , stroke "none"
        , fill "rgb(46, 118, 149)"
        ]
        []


view : List Float -> Svg msg
view model =
    let
        bins =
            Debug.log "bins" <| histogram model
    in
        svg [ width (toString w ++ "px"), height (toString h ++ "px") ]
            [ g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString (h - padding) ++ ")") ]
                [ xAxis model ]
            , g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString padding ++ ")") ]
                [ yAxis bins ]
            , g [ transform ("translate(" ++ toString padding ++ ", " ++ toString padding ++ ")"), class "series" ] <|
                List.map (column xScale (yScale bins)) bins
            ]


main =
    view model

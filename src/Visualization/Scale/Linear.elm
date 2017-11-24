module Visualization.Scale.Linear exposing (convert, invert, deinterpolate, ticks, tickFormat, nice, rangeExtent)

import Visualization.List as List
import Visualization.Scale.Internal exposing (bimap, interpolateFloat, toFixed)


rangeExtent : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
rangeExtent d r =
    r


nice ( start, stop ) count =
    let
        step0 =
            List.tickStep start stop count

        step1 =
            List.tickStep (toFloat (floor (start / step0)) * step0) (toFloat (ceiling (stop / step0)) * step0) count
    in
        ( toFloat (floor (start / step1)) * step1, toFloat (ceiling (stop / step1)) * step1 )


exponent x =
    if x == 0 then
        0
    else if x < 1 then
        1 + exponent (x * 10)
    else
        0


precisionFixed step =
    max 0 (exponent (abs step))


tickFormat ( start, stop ) count =
    List.tickStep start stop count
        |> precisionFixed
        |> toFixed


convert domain range =
    bimap domain range deinterpolate interpolateFloat


invert domain range =
    bimap range domain deinterpolate interpolate


deinterpolate a b x =
    let
        normalizedB =
            b - a
    in
        if normalizedB == 0 then
            0
        else
            (x - a) / normalizedB


ticks ( start, end ) count =
    List.ticks start end count


interpolate =
    interpolateFloat

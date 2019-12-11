module Scale.Linear exposing (convert, deinterpolate, invert, nice, rangeExtent, tickFormat, ticks)

import Scale.Internal exposing (bimap, interpolateFloat, toFixed)
import Statistics


rangeExtent : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
rangeExtent d r =
    r


nice ( start, stop ) count =
    let
        step0 =
            Statistics.tickStep start stop count

        step1 =
            Statistics.tickStep (toFloat (floor (start / step0)) * step0) (toFloat (ceiling (stop / step0)) * step0) count
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
    Statistics.tickStep start stop count
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
    Statistics.ticks start end count


interpolate =
    interpolateFloat

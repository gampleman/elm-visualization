module Scale.Linear exposing (convert, invert, nice, scale, tickFormat, ticks)

import Scale.Internal as Continuous
import Statistics


scale range_ domain_ =
    { domain = domain_
    , range = range_
    , convert = convert
    , invert = invert
    , ticks = ticks
    , tickFormat = tickFormat
    , nice = nice
    , rangeExtent = \_ r -> r
    }


convert =
    Continuous.convertTransform identity Continuous.interpolateFloat


invert =
    Continuous.invertTransform identity identity


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
        |> Continuous.toFixed


ticks ( start, end ) count =
    Statistics.ticks start end count

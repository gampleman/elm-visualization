module Visualization.Scale.Linear exposing (convert, invert, deinterpolate, ticks, tickFormat, nice, rangeExtent)

import Visualization.List as List
import Visualization.Interpolate exposing (interpolateFloat)
import Visualization.Scale.Internal exposing (bimap)


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


tickFormat _ _ =
    toString


convert domain range =
    bimap domain range deinterpolate interpolateFloat


invert domain range =
    bimap range domain deinterpolate interpolate


deinterpolate a b x =
    let
        b' =
            b - a
    in
        if b' == 0 then
            0
        else
            (x - a) / b'


ticks ( start, end ) count =
    List.ticks start end count


interpolate =
    interpolateFloat

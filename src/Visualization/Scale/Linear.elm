module Visualization.Scale.Linear exposing (convert, invert, deinterpolate, ticks, tickFormat, nice, rangeExtent)

import Visualization.List as List
import Visualization.Interpolate exposing (interpolateFloat)
import Visualization.Scale.Internal as Internal exposing (bimap)


rangeExtent : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
rangeExtent d r =
    r


nice = Internal.nice


tickFormat = Internal.tickFormat


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


ticks = Internal.ticks


interpolate =
    interpolateFloat

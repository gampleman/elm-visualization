module Visualization.Interpolate exposing (interpolateFloat)


interpolateFloat : Float -> Float -> Float -> Float
interpolateFloat from to time =
    from + (to - from) * time

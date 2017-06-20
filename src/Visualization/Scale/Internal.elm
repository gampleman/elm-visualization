module Visualization.Scale.Internal exposing (bimap, ticks, tickFormat, nice)

import Visualization.List as List

bimap :
    ( comparable0, comparable0 )
    -> ( comparable1, comparable1 )
    -> (comparable0 -> comparable0 -> a -> b)
    -> (comparable1 -> comparable1 -> b -> c)
    -> (a -> c)
bimap ( d0, d1 ) ( r0, r1 ) deinterpolate reinterpolate =
    let
        ( de, re ) =
            if d1 < d0 then
                ( deinterpolate d1 d0, reinterpolate r1 r0 )
            else
                ( deinterpolate d0 d1, reinterpolate r0 r1 )
    in
        re << de

ticks ( start, end ) count =
    List.ticks start end count

tickFormat _ _ =
    toString

nice ( start, stop ) count =
    let
        step0 =
            List.tickStep start stop count

        step1 =
            List.tickStep (toFloat (floor (start / step0)) * step0) (toFloat (ceiling (stop / step0)) * step0) count
    in
        ( toFloat (floor (start / step1)) * step1, toFloat (ceiling (stop / step1)) * step1 )

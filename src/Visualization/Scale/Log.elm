module Visualization.Scale.Log exposing (convert, invert, nice, rangeExtent, tickFormat, ticks)

import Visualization.List as List
import Visualization.Scale.Internal exposing (bimap, interpolateFloat)
import Visualization.Scale.Linear


rangeExtent : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
rangeExtent d r =
    r


log =
    logBase e


convert domain range =
    bimap domain range deinterpolate interpolateFloat


invert domain range =
    bimap range domain deinterpolate interpolate


deinterpolate a b x =
    log (x / a) / log (b / a)


interpolate a b x =
    if a < 0 then
        -b ^ x * -a ^ (1 - x)
    else
        b ^ x * a ^ (1 - x)


ticks : Float -> ( Float, Float ) -> Int -> List Float
ticks base ( start, end ) count =
    let
        topi =
            logBase base start

        topj =
            logBase base end

        n =
            toFloat count

        ticksHelper inc st i j k =
            let
                p =
                    i ^ base

                t =
                    p * k
            in
            if st k then
                if t < start then
                    ticksHelper inc st i j (inc k)
                else if t > end then
                    []
                else
                    ticksHelper inc st i j (inc k) ++ [ t ]
            else
                []
    in
    if not (toFloat (round base) == base) && topj - topi < n then
        if start > 0 then
            ticksHelper (\k -> k + 1) (\k -> k < base) (toFloat (round topi - 1)) (toFloat (round topj + 1)) 1
        else
            ticksHelper (\k -> k - 1) (\k -> k >= 1) (toFloat (round topi - 1)) (toFloat (round topj + 1)) (base - 1)
    else
        List.map (\a -> a ^ base) <| List.ticks topi topj <| round (min (topj - topi) n)


tickFormat =
    Visualization.Scale.Linear.tickFormat


nice base ( start, stop ) _ =
    let
        f =
            (\a -> a ^ base) << toFloat << floor << logBase base

        c =
            (\a -> a ^ base) << toFloat << ceiling << logBase base
    in
    ( f start, c stop )

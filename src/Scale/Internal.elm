module Scale.Internal exposing (bimap, interpolateFloat, toFixed)

import String exposing (join, padRight, split)


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


interpolateFloat : Float -> Float -> Float -> Float
interpolateFloat from to time =
    from + (to - from) * time


toFixed : Int -> Float -> String
toFixed precision value =
    let
        power =
            toFloat 10 ^ toFloat precision

        pad num =
            case num of
                [ x, y ] ->
                    [ x, String.padRight precision '0' y ]

                [ val ] ->
                    if precision > 0 then
                        [ val, String.padRight precision '0' "" ]

                    else
                        [ val ]

                val ->
                    val
    in
    (round (value * power) |> toFloat)
        / power
        |> String.fromFloat
        |> String.split "."
        |> pad
        |> String.join "."

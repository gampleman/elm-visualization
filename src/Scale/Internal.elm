module Scale.Internal exposing (convertTransform, interpolateFloat, invertTransform, toFixed)

import String exposing (join, padRight, split)


normalize : Float -> Float -> Float -> Float
normalize a b =
    let
        c =
            b - a
    in
    if c == 0 then
        always 0.5

    else if isNaN c then
        always (0 / 0)

    else
        \x -> (x - a) / c


bimap : ( Float, Float ) -> ( a, a ) -> (a -> a -> Float -> a) -> (Float -> a)
bimap ( d0, d1 ) ( r0, r1 ) interpolate =
    let
        ( de, re ) =
            if d1 < d0 then
                ( normalize d1 d0, interpolate r1 r0 )

            else
                ( normalize d0 d1, interpolate r0 r1 )
    in
    re << de


convertTransform : (b -> Float) -> (a -> a -> Float -> a) -> ( b, b ) -> ( a, a ) -> (b -> a)
convertTransform transform interpolate ( d0, d1 ) range =
    transform >> bimap ( transform d0, transform d1 ) range interpolate


invertTransform : (b -> Float) -> (Float -> b) -> ( b, b ) -> ( Float, Float ) -> (Float -> b)
invertTransform transform untransform ( d0, d1 ) range =
    bimap range ( transform d0, transform d1 ) interpolateFloat >> untransform


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

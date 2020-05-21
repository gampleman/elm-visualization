module Scale.Continuous exposing (convertTransform, invertTransform, linear, nice, power, radial, symlog, tickFormat, transformPow, transformSymlog)

import Interpolation
import Statistics
import String exposing (join, padRight, split)



-- Transforms


transformPow expon x =
    if x < 0 then
        -(-x ^ expon)

    else
        x ^ expon


transformSymlog c x =
    if x < 0 then
        -(logBase e (abs (x / c) + 1))

    else
        logBase e (abs (x / c) + 1)


transformSymexp c x =
    if x < 0 then
        -(e ^ -x - 1) * c

    else
        (e ^ x - 1) * c



-- Scales


linear =
    scaleWithTransform identity identity


power expo =
    scaleWithTransform (transformPow expo) (transformPow (1 / expo))


symlog c =
    scaleWithTransform (transformSymlog c) (transformSymexp c)



-- Radial


radial range_ domain_ =
    { domain = domain_
    , range = range_
    , convert = \d r -> convertTransform identity Interpolation.float d (squareRange r) >> unsquare
    , invert = \d r -> square >> invertTransform identity identity d (squareRange r)
    , ticks = ticks
    , tickFormat = tickFormat
    , nice = nice
    , rangeExtent = \_ r -> r
    }


withSquaredRange f domain ( r0, r1 ) =
    f domain ( square r0, square r1 )


squareRange ( a, b ) =
    ( square a, square b )


square : Float -> Float
square x =
    (if x >= 0 then
        1

     else
        -1
    )
        * x
        * x


unsquare x =
    (if x >= 0 then
        1

     else
        -1
    )
        * sqrt (abs x)



-- Basics


scaleWithTransform transform untransform range_ domain_ =
    { domain = domain_
    , range = range_
    , convert = convertTransform transform Interpolation.float
    , invert = invertTransform transform untransform
    , ticks = ticks
    , tickFormat = tickFormat
    , nice = nice
    , rangeExtent = \_ r -> r
    }


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


ticks ( start, end ) count =
    Statistics.ticks start end count



-- Internal implementation details


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
    bimap range ( transform d0, transform d1 ) Interpolation.float >> untransform


toFixed : Int -> Float -> String
toFixed precision value =
    let
        power_ =
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
    (round (value * power_) |> toFloat)
        / power_
        |> String.fromFloat
        |> String.split "."
        |> pad
        |> String.join "."

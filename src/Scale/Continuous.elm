module Scale.Continuous exposing (convertTransform, invertTransform, linear, nice, power, radial, symlog, tickFormat, transformPow, transformSymlog)

import Interpolation
import Statistics
import String



-- Transforms


transformPow : Float -> Float -> Float
transformPow expon x =
    if x < 0 then
        -(-x ^ expon)

    else
        x ^ expon


transformSymlog : Float -> Float -> Float
transformSymlog c x =
    if x < 0 then
        -(logBase e (abs (x / c) + 1))

    else
        logBase e (abs (x / c) + 1)


transformSymexp : Float -> Float -> Float
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


squareRange : ( Float, Float ) -> ( Float, Float )
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


unsquare : Float -> Float
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


ln10 : Float
ln10 =
    logBase e 10


e10 : Float
e10 =
    sqrt 50


e5 : Float
e5 =
    sqrt 10


e2 : Float
e2 =
    sqrt 2


tickIncrement : Float -> Float -> Int -> Float
tickIncrement start stop count =
    let
        step =
            (stop - start) / max 0 (toFloat count)

        powr =
            toFloat (floor (logBase e step / ln10))

        error =
            step / (10 ^ powr)

        order =
            if error >= e10 then
                10

            else if error >= e5 then
                5

            else if error >= e2 then
                2

            else
                1
    in
    if powr >= 0 then
        order * (10 ^ powr)

    else
        -(10 ^ -powr) / order


{-| Applies the function ensuring that the domain is sorted ascending, but then returns the result in the original order.

    foo (a, b) = ( a * 2, b * 3)

    foo (1, 2)                      --> (2, 6)
    withNormalizedDomain foo (1, 2) --> (2, 6)
    foo (2, 1)                      --> (4, 3)
    withNormalizedDomain foo (2, 1) --> (6, 2)

-}
withNormalizedDomain : (( comparable, comparable ) -> ( comparable, comparable )) -> ( comparable, comparable ) -> ( comparable, comparable )
withNormalizedDomain fn ( a, b ) =
    if a < b then
        fn ( a, b )

    else
        let
            ( d, c ) =
                fn ( b, a )
        in
        ( c, d )


{-| Computes a fixpoint on the first return value `Float`, with a bounded number of iterations
-}
fixPoint : Int -> b -> (b -> ( Float, b )) -> b
fixPoint maxIterations initialInput fn =
    let
        helper iters ( a, b ) =
            if iters + 1 >= maxIterations then
                b

            else
                let
                    ( outA, outB ) =
                        fn b
                in
                if outA == a then
                    b

                else if outA == 0 then
                    b

                else
                    helper (iters + 1) ( outA, outB )
    in
    helper 1 (fn initialInput)


nice domain count =
    let
        computation ( start, stop ) =
            let
                step =
                    tickIncrement start stop count
            in
            ( step
            , if step > 0 then
                ( toFloat (floor (start / step)) * step, toFloat (ceiling (stop / step)) * step )

              else if step < 0 then
                ( toFloat (ceiling (start * step)) / step, toFloat (floor (stop * step)) / step )

              else
                ( start, stop )
            )
    in
    withNormalizedDomain (\dmn -> fixPoint 10 dmn computation) domain


exponent num =
    let
        helper soFar x =
            if x == 0 then
                soFar

            else if x < 1 then
                helper (1 + soFar) (x * 10)

            else
                soFar
    in
    helper 0 num


precisionFixed step =
    max 0 (exponent (abs step))


tickFormat ( start, stop ) count =
    Statistics.tickStep start stop count
        |> precisionFixed
        |> toFixed


ticks : ( Float, Float ) -> Int -> List Float
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

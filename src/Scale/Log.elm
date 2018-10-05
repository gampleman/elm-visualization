module Scale.Log exposing (convert, invert, nice, rangeExtent, tickFormat, ticks)

import Scale.Internal exposing (bimap, interpolateFloat)
import Scale.Linear
import Statistics


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


makePows start base =
    if start < 0 then
        \x -> -(base ^ -x)

    else
        \x -> base ^ x


makeLogs start base =
    if start < 0 then
        \x -> -(logBase base -x)

    else
        logBase base


ticks : Float -> ( Float, Float ) -> Int -> List Float
ticks base ( domStart, domEnd ) count =
    let
        ( start, end, reverse ) =
            if domStart <= domEnd then
                ( domStart, domEnd, identity )

            else
                ( domEnd, domStart, List.reverse )

        ( topi, topj ) =
            if start < 0 then
                ( -(logBase base -start), -(logBase base -end) )

            else
                ( logBase base start, logBase base end )

        pows =
            makePows start base

        n =
            toFloat count

        positiveHelper i j k =
            let
                p =
                    pows i

                t =
                    p * k
            in
            if i < j then
                if k < base then
                    if t < start then
                        positiveHelper i j (k + 1)

                    else if t > end then
                        positiveHelper (i + 1) j 1

                    else
                        t :: positiveHelper i j (k + 1)

                else
                    positiveHelper (i + 1) j 1

            else
                []

        negativeHelper i j k =
            let
                p =
                    pows i

                t =
                    p * k
            in
            if i < j then
                if k >= 1 then
                    if t < start then
                        negativeHelper i j (k - 1)

                    else if t > end then
                        negativeHelper (i + 1) j (base - 1)

                    else
                        t :: negativeHelper i j (k - 1)

                else
                    negativeHelper (i + 1) j (base - 1)

            else
                []
    in
    if toFloat (round base) == base && topj - topi < n then
        if start > 0 then
            reverse <| positiveHelper (toFloat (round topi - 1)) (toFloat (round topj + 1)) 1

        else
            reverse <| negativeHelper (toFloat (round topi - 1)) (toFloat (round topj + 1)) (base - 1)

    else
        reverse <| List.map (\a -> pows a) <| Statistics.ticks topi topj <| round (min (topj - topi) n)


tickFormat base ( start, end ) count =
    let
        k =
            max 1 (base * toFloat count / toFloat (List.length (ticks base ( start, end ) 10)))

        pows =
            makePows start base

        logs =
            makeLogs start base
    in
    \d ->
        let
            i =
                d / pows (toFloat (round (logs d)))

            p =
                if i * base < base - 0.5 then
                    i * base

                else
                    i
        in
        if p <= k then
            if base == 10 then
                formatExponential d

            else
                formatFixed 12 d

        else
            ""


formatFixed : Int -> Float -> String
formatFixed precision value =
    let
        floored =
            floor value

        digits =
            if floored == 0 then
                0

            else
                floored
                    |> String.fromInt
                    |> String.length

        power =
            10 ^ toFloat (precision - digits)
    in
    (round (value * power) |> toFloat)
        / power
        |> String.fromFloat


formatExponential num =
    let
        parts =
            String.fromFloat num |> String.split "."

        ( digits0, decimals0 ) =
            case parts of
                [ a, b ] ->
                    ( a, b )

                [ a ] ->
                    ( a, "0" )

                _ ->
                    ( "0", "0" )

        helper level digits decimals =
            case ( digits, decimals ) of
                ( [ '0' ], x :: xs ) ->
                    helper (level - 1) [ x ] xs

                ( [ dig ], dec ) ->
                    String.cons dig "e"
                        ++ (if level >= 0 then
                                "+"

                            else
                                "-"
                           )
                        ++ String.fromInt (abs level)

                ( x :: xs, dec ) ->
                    helper (level + 1) xs (x :: dec)

                ( [], x :: xs ) ->
                    helper (level - 1) [ x ] xs

                ( [], [] ) ->
                    ""
    in
    helper 0 (String.toList digits0 |> List.reverse) (String.toList decimals0)


nice base ( start, stop ) _ =
    let
        f =
            (\a -> a ^ base) << toFloat << floor << logBase base

        c =
            (\a -> a ^ base) << toFloat << ceiling << logBase base
    in
    ( f start, c stop )

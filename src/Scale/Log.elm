module Scale.Log exposing (scale)

import Interpolation
import List.Extra
import Scale.Continuous as Continuous
import Statistics


scale base range_ domain_ =
    let
        ( transform, untransform ) =
            if Tuple.first domain_ < 0 then
                ( reflect log, reflect exp )

            else
                ( log, exp )
    in
    { domain = domain_
    , range = range_
    , convert = Continuous.convertTransform transform Interpolation.float
    , invert = Continuous.invertTransform transform untransform
    , ticks = ticks base
    , tickFormat = tickFormat base
    , nice = nice base
    , rangeExtent = \_ r -> r
    }


exp : Float -> Float
exp n =
    e ^ n


reflect : (Float -> Float) -> Float -> Float
reflect f =
    negate >> f >> negate


log : Float -> Float
log =
    logBase e


makePows : number -> number -> number -> number
makePows start base =
    if start < 0 then
        \x -> -(base ^ -x)

    else
        \x -> base ^ x


makeLogs : number -> Float -> Float -> Float
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
                ( domStart, domEnd, List.reverse )

            else
                ( domEnd, domStart, identity )

        ( topi, topj ) =
            if start < 0 then
                ( -(logBase base -start), -(logBase base -end) )

            else
                ( logBase base start, logBase base end )

        pows =
            makePows start base

        n =
            toFloat count

        positiveHelper i j k res =
            if i < j then
                if k < base then
                    let
                        p =
                            pows i

                        t =
                            p * k
                    in
                    if t < start then
                        positiveHelper i j (k + 1) res

                    else if t > end then
                        positiveHelper (i + 1) j 1 res

                    else
                        positiveHelper i j (k + 1) (t :: res)

                else
                    positiveHelper (i + 1) j 1 res

            else
                reverse res

        negativeHelper i j k res =
            if i < j then
                if k >= 1 then
                    let
                        p =
                            pows i

                        t =
                            p * k
                    in
                    if t < start then
                        negativeHelper i j (k - 1) res

                    else if t > end then
                        negativeHelper (i + 1) j (base - 1) res

                    else
                        negativeHelper i j (k - 1) (t :: res)

                else
                    negativeHelper (i + 1) j (base - 1) res

            else
                reverse res
    in
    if toFloat (round base) == base && topj - topi < n then
        if start > 0 then
            positiveHelper (toFloat (round topi - 1)) (toFloat (round topj + 1)) 1 []

        else
            negativeHelper (toFloat (round topi - 1)) (toFloat (round topj + 1)) (base - 1) []

    else
        reverse <| List.Extra.reverseMap (\a -> pows a) <| Statistics.ticks topi topj <| round (min (topj - topi) n)


tickFormat : Float -> ( Float, Float ) -> Int -> Float -> String
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


formatExponential : Float -> String
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

                ( [ dig ], _ ) ->
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


nice : Float -> ( Float, Float ) -> c -> ( Float, Float )
nice base ( start, stop ) _ =
    let
        f =
            (\a -> a ^ base) << toFloat << floor << logBase base

        c =
            (\a -> a ^ base) << toFloat << ceiling << logBase base
    in
    ( f start, c stop )

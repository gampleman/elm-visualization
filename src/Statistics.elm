module Statistics exposing
    ( extent, extentBy, extentWith
    , variance, deviation, quantile
    , ticks, tickStep, range
    )

{-|

@docs extent, extentBy, extentWith

@docs variance, deviation, quantile


# Transformations

Methods for transforming list and for generating new lists.

@docs ticks, tickStep, range

-}

import Bitwise
import List.Extra


{-| Returns a List containing an arithmetic progression, similar to the Python
built-in range. This method is often used to iterate over a sequence of
uniformly-spaced numeric values, such as the indexes of an array or the ticks of
a linear scale. (See also [ticks](#ticks) for nicely-rounded values.)

Takes a `start`, `stop` and `step` argument. The stop value is exclusive; it is not
included in the result. If `step` is positive, the last element is the largest
`start + i * step` less than `stop`; if `step` is negative, the last element is
the smallest `start + i * step` greater than `stop`. If the returned list would
contain an infinite number of values, an empty range is returned.

The arguments are not required to be whole numbers; however, the results are more
predictable if they are.

Differences from [List.range from the standard library](https://package.elm-lang.org/packages/elm/core/latest/List#range):

  - `List.range` is inclusive, meaning that the stop value will be included in the result
  - `List.range` supports `Int`, whereas this uses `Float`
  - `List.range` supports only increasing intervals (i.e. `List.range 3 1 == []` vs. `range 3 1 -1 == [3, 2]`)
  - `List.range` doesn't allow for specifying the step value

-}
range : Float -> Float -> Float -> List Float
range start stop step =
    let
        n =
            (stop - start)
                / step
                |> ceiling
                -- get rid of NaN
                |> Bitwise.or 0
                |> max 0

        helper i list =
            if i >= 0 then
                helper (i - 1) <| start + step * toFloat i :: list

            else
                list
    in
    helper (n - 1) []


{-| Returns a list of approximately n + 1 uniformly-spaced, nicely-rounded
values between a start and stop value (inclusive). Each value is a power of ten
multiplied by 1, 2 or 5. Note that due to the limited precision of IEEE 754
floating point, the returned values may not be exact decimals.

Ticks are inclusive in the sense that they may include the specified start and
stop values if (and only if) they are exact, nicely-rounded values consistent
with the inferred step. More formally, each returned tick t satisfies
start ≤ t and t ≤ stop.

    ticks 1.9 6.4 10 --> [2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6]

    ticks 1.9 6 5 --> [2, 3, 4, 5, 6]

-}
ticks : Float -> Float -> Int -> List Float
ticks start stop count =
    let
        step =
            tickStep start stop count

        beg =
            toFloat (ceiling (start / step)) * step

        end =
            toFloat (floor (stop / step)) * step + step / 2
    in
    range beg end step


{-| Returns the difference between adjacent tick values if the same arguments
were passed to `ticks`: a nicely-rounded value that is a power of ten multiplied
by 1, 2 or 5. Note that due to the limited precision of IEEE 754 floating point,
the returned value may not be exact decimals.

    tickStep 1.9 6.4 10 -- 0.5

    tickStep 1.9 6 5 -- 1

-}
tickStep : Float -> Float -> Int -> Float
tickStep start stop count =
    let
        step0 =
            abs (stop - start) / max 0 (toFloat count)

        step1 =
            toFloat (10 ^ floor (logBase e step0 / logBase e 10))

        error =
            step0 / step1

        step2 =
            if error >= sqrt 50 then
                step1 * 10

            else if error >= sqrt 10 then
                step1 * 5

            else if error >= sqrt 2 then
                step1 * 2

            else
                step1
    in
    if stop < start then
        -step2

    else
        step2


{-| Returns the minimum and maximum value in the list.
-}
extent : List comparable -> Maybe ( comparable, comparable )
extent =
    extentBy identity


{-| Returns the minimum and maximum value in the given array using comparisons
from values passed by the accessor function.

    data : List { name : String, age : Int}
    data =
      [ {name = "John Smith", age = 32 }
      , {name = "Mark Luther", age = 45 }
      , {name = "Cory Jones", age = 26 }
      ]

    extentBy .age data
    --> Just ({name = "Cory Jones", age = 26 }
    -->      , {name = "Mark Luther", age = 45 })

-}
extentBy : (a -> comparable) -> List a -> Maybe ( a, a )
extentBy fn list =
    let
        min a b =
            if fn a < fn b then
                a

            else
                b

        max a b =
            if fn a > fn b then
                a

            else
                b

        helper l ( mini, maxi ) =
            case l of
                [] ->
                    ( mini, maxi )

                x :: xs ->
                    helper xs ( min mini x, max maxi x )
    in
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just <| helper xs ( x, x )


{-| Returns the minimum and maximum value in the given array using comparisons
provided by the comparison function.
-}
extentWith : (a -> a -> Order) -> List a -> Maybe ( a, a )
extentWith toOrder list =
    let
        max a b =
            case toOrder a b of
                GT ->
                    a

                _ ->
                    b

        folder element ( mini, maxi ) =
            case toOrder element mini of
                LT ->
                    -- if new is less than mini, it can never be larger than maxi
                    -- so we're immediately done
                    ( element, maxi )

                EQ ->
                    -- idem
                    ( element, maxi )

                GT ->
                    ( mini, max element maxi )
    in
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just <| List.foldl folder ( x, x ) xs


{-| Returns an [unbiased estimator of the population variance](http://mathworld.wolfram.com/SampleVariance.html) of the
given list of numbers. If the list has fewer than two values, returns Nothing.
-}
variance : List Float -> Maybe Float
variance nums =
    let
        compute value ( mean, i, sm ) =
            let
                delta =
                    value - mean

                newMean =
                    mean + delta / (i + 1)
            in
            ( newMean, i + 1, sm + delta * (value - newMean) )

        ( _, length, sum ) =
            List.foldr compute ( 0, 0, 0 ) nums
    in
    if length > 1 then
        Just (sum / (length - 1))

    else
        Nothing


{-| Returns the standard deviation, defined as the square root of the [bias-corrected variance](#variance), of the given
list of numbers. If the list has fewer than two values, returns Nothing.
-}
deviation : List Float -> Maybe Float
deviation =
    variance >> Maybe.map sqrt


{-| Returns the p-quantile of the given **sorted** list of numbers, where `p` is a number in the range [0, 1]. For
example, the median can be computed using p = 0.5, the first quartile at p = 0.25, and the third quartile at p = 0.75.
This particular implementation uses the [R-7 method](https://en.wikipedia.org/wiki/Quantile#Quantiles_of_a_population),
which is the default for the R programming language and Excel. For example:

    a : List Float
    a = [0, 10, 30]

    quantile 0 a --> Just 0
    quantile 0.5 a --> Just 10
    quantile 1 a --> Just 30
    quantile 0.25 a --> Just 5
    quantile 0.75 a --> Just 20
    quantile 0.1 a --> Just 2

-}
quantile : Float -> List Float -> Maybe Float
quantile p values =
    if p <= 0 then
        List.head values

    else if p >= 1 then
        List.Extra.last values

    else
        case values of
            [] ->
                Nothing

            [ head ] ->
                Just head

            x :: y :: tail ->
                let
                    n =
                        List.length values |> toFloat

                    i =
                        (n - 1) * p

                    i0 =
                        floor i

                    value0 =
                        List.Extra.getAt i0 values |> Maybe.withDefault x

                    value1 =
                        List.Extra.getAt (i0 + 1) values |> Maybe.withDefault y
                in
                Just <| value0 + (value1 - value0) * (i - toFloat i0)

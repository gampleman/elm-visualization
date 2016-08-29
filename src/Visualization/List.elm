module Visualization.List exposing (..)

{-| Visualization List Utilities

This module exposes functions on list which are useful for the domain of data
visualization. Most of these work with Lists of numbers.

## Transformations

Methods for transforming arrays and for generating new arrays.

@docs ticks, tickStep, range
-}


{-| Returns a List containing an arithmetic progression, similar to the Python
built-in range. This method is often used to iterate over a sequence of
uniformly-spaced numeric values, such as the indexes of an array or the ticks of
a linear scale. (See also [ticks](#ticks) for nicely-rounded values.)

Takes a `start`, `stop` and `step` argument. The stop value is exclusive; it is not
included in the result. If `step` is positive, the last element is the largest
`start + i * step` less than `stop`; if `step` is negative, the last element is
the smallest `start + i * step` greater than `stop`. If the returned list would
contain an infinite number of values, an empty range is returned.

The arguments are not required to be integers; however, the results are more
predictable if they are.
-}
range : number -> number -> number -> List number
range start stop step =
    let
        range' s list =
            if s + step > stop then
                list ++ [ s ]
            else
                range' (s + step) (list ++ [ s ])
    in
        range' start []


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


tickStep : Float -> Float -> Int -> Float
tickStep start stop count =
    let
        step0 =
            abs (stop - start) / max 0 (toFloat count)

        step1 =
            toFloat (10 ^ (floor (logBase e step0 / logBase e 10)))

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


extent : List comparable -> Maybe ( comparable, comparable )
extent =
    extentWith identity


extentWith : (a -> comparable) -> List a -> Maybe ( a, a )
extentWith fn list =
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

        helper list ( mini, maxi ) =
            case list of
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

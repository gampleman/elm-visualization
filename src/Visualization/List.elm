module Visualization.List exposing (range, ticks, tickStep, extent, extentWith)

{-| This module exposes functions on list which are useful for the domain of data
visualization. Most of these work with Lists of numbers.


# Statistics

@docs extent, extentWith


# Transformations

Methods for transforming list and for generating new lists.

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

The arguments are not required to be whole numbers; however, the results are more
predictable if they are.

Differences from [List.range from the standard library](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/List#range):

  - `List.range` is inclusive, meaning that the stop value will be included in the result
  - `List.range` only supports `Int`, whereas this supports any number types
  - `List.range` supports only increasing intervals (i.e. `List.range 3 1 == []` vs. `range 3 1 -1 == [3, 2]`).
  - `List.range` doesn't allow for specifying the step value
  - `List.range` is most likely faster

-}
range : number -> number -> number -> List number
range begin end step =
    if gt (end - begin) && gt step then
        rangePositive begin end step
    else if gt (begin - end) && gt -step then
        rangeNegative begin end step
    else
        []


gt n =
    n > 0


rangePositive begin stop step =
    let
        helper s list =
            if s == stop then
                list
            else if gt (s + step - stop) then
                s :: list
            else
                helper (s + step) (s :: list)
    in
        helper begin [] |> List.reverse


rangeNegative begin stop step =
    let
        helper s list =
            if s == stop then
                list
            else if gt (stop - (s + step)) then
                s :: list
            else
                helper (s + step) (s :: list)
    in
        helper begin [] |> List.reverse



{-
   -- This a more efficient and correct version, however it assumes the arguments are Floats:

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
                   helper (i - 1) <| start + step * i :: list
               else
                   list
       in
           helper (n - 1) []
-}


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

    tickStep 1.9 6.4 10 --> 0.5

    tickStep 1.9 6 5 --> 1

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
    extentWith identity


{-| Returns the minimum and maximum value in the given array using comparisons
from values passed by the accessor function.

    data : List { name : String, age : Int}
    data =
      [ {name = "John Smith", age = 32 }
      , {name = "Mark Luther", age = 45 }
      , {name = "Cory Jones", age = 26 }
      ]

    extentWith .age data
    --> Just ({name = "Cory Jones", age = 26 }
    -->      , {name = "Mark Luther", age = 45 })

-}
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

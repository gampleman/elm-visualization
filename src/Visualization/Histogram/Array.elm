module Visualization.Histogram.Array exposing (bisectRight)

import Array exposing (Array)
import Bitwise


bisectRight : comparable -> Array comparable -> Maybe ( Int, Int ) -> Int
bisectRight item array extent =
    let
        -- we start by doing a bounds check, so this shouldn't fail
        usafeGet index =
            case Array.get index array of
                Just el ->
                    el

                Nothing ->
                    Debug.crash "An invariant in bisectRight was breached. This is a bug, please report this to https://github.com/gampleman/elm-visualization/issues"

        helper lo hi =
            if lo < hi then
                let
                    mid =
                        Bitwise.shiftRightZfBy 1 (lo + hi)
                in
                    if usafeGet mid >= item then
                        helper lo mid
                    else
                        helper (mid + 1) hi
            else
                lo
    in
        case extent of
            Just ( lo, hi ) ->
                helper (max 0 lo) (min hi <| Array.length array)

            Nothing ->
                helper 0 <| Array.length array

module Histogram.Array exposing (bisectRight)

import Array exposing (Array)
import Bitwise


bisectRight : comparable -> Array comparable -> Maybe ( Int, Int ) -> Int
bisectRight item array extent =
    case Array.get 0 array of
        Nothing ->
            0

        Just default ->
            let
                -- we start by doing a bounds check, so this shouldn't fail
                get index =
                    Array.get index array |> Maybe.withDefault default

                helper lo hi =
                    if lo < hi then
                        let
                            mid =
                                Bitwise.shiftRightZfBy 1 (lo + hi)
                        in
                        if get mid >= item then
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

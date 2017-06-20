module Visualization.Scale.Quantize exposing (convert, invertExtent, ticks, tickFormat, nice, rangeExtent)

import Visualization.List as VList
import Visualization.Scale.Linear as Linear


rangeExtent : ( Float, Float ) -> ( a, List a ) -> ( a, a )
rangeExtent ( mi, ma ) range =
    ( convert ( mi, ma ) range mi, convert ( mi, ma ) range ma )


computeDomain : ( Float, Float ) -> List a -> List Float
computeDomain ( mi, ma ) tail =
    let
        l =
            List.length tail

        step =
            (ma - mi) / toFloat (l + 1)
    in
        Maybe.withDefault [ 0 ] <| List.tail <| VList.range mi ma step


convert : ( Float, Float ) -> ( a, List a ) -> Float -> a
convert domain ( head, tail ) val =
    let
        last head tail =
            case tail of
                [] ->
                    head

                x :: xs ->
                    last x xs

        helper dom range =
            case dom of
                [] ->
                    case range of
                        [] ->
                            last head tail

                        r :: _ ->
                            r

                d :: ds ->
                    case range of
                        [] ->
                            Debug.crash "Invariant breached: ran out of range"

                        r :: rs ->
                            if val > d then
                                helper ds rs
                            else
                                r
    in
        helper (computeDomain domain tail) (head :: tail)


invertExtent : ( Float, Float ) -> ( a, List a ) -> a -> Maybe ( Float, Float )
invertExtent ( mi, ma ) ( head, tail ) val =
    let
        domain =
            computeDomain ( mi, ma ) tail

        helper domain range =
            case range of
                [] ->
                    Nothing

                x :: xs ->
                    if x == val then
                        case domain of
                            a :: b :: _ ->
                                Just ( a, b )

                            _ ->
                                Nothing
                    else
                        case domain of
                            [] ->
                                Nothing

                            d :: ds ->
                                helper ds xs
    in
        helper (mi :: domain ++ [ ma ]) (head :: tail)


ticks : ( Float, Float ) -> ( a, List a ) -> Int -> List Float
ticks ( start, end ) domain count =
    VList.ticks start end count


tickFormat : ( Float, Float ) -> ( a, List a ) -> Int -> Float -> String
tickFormat _ _ _ =
    toString


nice : ( Float, Float ) -> Int -> ( Float, Float )
nice =
    Linear.nice

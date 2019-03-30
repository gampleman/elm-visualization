module Scale.Quantize exposing (convert, invertExtent, nice, rangeExtent, tickFormat, ticks)

import Scale.Linear as Linear
import Statistics


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
    Maybe.withDefault [ 0 ] <| List.tail <| Statistics.range mi ma step


convert : ( Float, Float ) -> ( a, List a ) -> Float -> a
convert domain ( head, tail ) val =
    let
        last h t =
            case tail of
                [] ->
                    h

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
                            head

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

        helper dmn range =
            case range of
                [] ->
                    Nothing

                x :: xs ->
                    if x == val then
                        case dmn of
                            a :: b :: _ ->
                                Just ( a, b )

                            _ ->
                                Nothing

                    else
                        case dmn of
                            [] ->
                                Nothing

                            d :: ds ->
                                helper ds xs
    in
    helper (mi :: domain ++ [ ma ]) (head :: tail)


ticks : ( Float, Float ) -> ( a, List a ) -> Int -> List Float
ticks ( start, end ) domain count =
    Statistics.ticks start end count


tickFormat : ( Float, Float ) -> Int -> Float -> String
tickFormat =
    Linear.tickFormat


nice : ( Float, Float ) -> Int -> ( Float, Float )
nice =
    Linear.nice

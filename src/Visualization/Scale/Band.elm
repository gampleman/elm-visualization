module Visualization.Scale.Band exposing (convert, bandwitdh)

import Json.Decode exposing (index)


bandwitdh : { paddingInner : Float, paddingOuter : Float, align : Float } -> List a -> ( Float, Float ) -> Float
bandwitdh { paddingInner, paddingOuter, align } domain ( d0, d1 ) =
    let
        ( start, stop ) =
            if d0 < d1 then
                ( d0, d1 )
            else
                ( d1, d0 )

        n =
            toFloat <| List.length domain

        step =
            (stop - start) / max 1 (n - paddingInner + paddingOuter * 2)
    in
        step * (1 - paddingInner)


convert : { paddingInner : Float, paddingOuter : Float, align : Float } -> List a -> ( Float, Float ) -> a -> Float
convert { paddingInner, paddingOuter, align } domain ( start, stop ) value =
    case indexOf value domain of
        Just index ->
            if start < stop then
                let
                    n =
                        toFloat <| List.length domain

                    step =
                        (stop - start) / max 1 (n - paddingInner + paddingOuter * 2)

                    start2 =
                        start + (stop - start - step * (n - paddingInner)) * align
                in
                    start + step * index
            else
                let
                    n =
                        toFloat <| List.length domain

                    step =
                        (start - stop) / max 1 (n - paddingInner + paddingOuter * 2)

                    stop2 =
                        stop + (start - stop - step * (n - paddingInner)) * align
                in
                    stop2 + step * (n - index - 1)

        Nothing ->
            0 / 0


indexOf =
    indexOfHelp 0


indexOfHelp index value list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if value == x then
                Just index
            else
                indexOfHelp (index + 1) value xs

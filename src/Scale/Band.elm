module Scale.Band exposing (bandwidth, convert)

import Json.Decode exposing (index)


type alias Config =
    { paddingInner : Float, paddingOuter : Float, align : Float }


normalizeConfig : Config -> Config
normalizeConfig { paddingInner, paddingOuter, align } =
    { paddingInner = clamp 0 1 paddingInner
    , paddingOuter = clamp 0 1 paddingOuter
    , align = clamp 0 1 align
    }


bandwidth : Config -> List a -> ( Float, Float ) -> Float
bandwidth cfg domain ( d0, d1 ) =
    let
        { paddingInner, paddingOuter, align } =
            normalizeConfig cfg

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


computePositions index cfg n ( start, stop ) =
    let
        { paddingInner, paddingOuter, align } =
            normalizeConfig cfg

        step =
            (stop - start) / max 1 (n - paddingInner + paddingOuter * 2)

        start2 =
            start + (stop - start - step * (n - paddingInner)) * align
    in
    ( start2, step )


convert : Config -> List a -> ( Float, Float ) -> a -> Float
convert cfg domain ( start, stop ) value =
    case indexOf value domain of
        Just index ->
            let
                n =
                    toFloat <| List.length domain
            in
            if start < stop then
                let
                    ( start2, step ) =
                        computePositions index cfg n ( start, stop )
                in
                start2 + step * index

            else
                let
                    ( stop2, step ) =
                        computePositions index cfg n ( stop, start )
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

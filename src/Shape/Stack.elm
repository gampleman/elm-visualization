module Shape.Stack exposing
    ( computeStack
    , offsetDiverging
    , offsetExpand
    , offsetNone
    , offsetSilhouette
    , offsetWiggle
    , sortByInsideOut
    )

import List.Extra as List


computeStack :
    { d
        | data : b
        , offset : List (List ( number, c )) -> List (List ( Float, Float ))
        , order : b -> List ( a, List c )
    }
    ->
        { extent : ( Float, Float )
        , labels : List a
        , values : List (List ( Float, Float ))
        }
computeStack { offset, order, data } =
    let
        ( labels, values ) =
            data
                |> order
                |> List.unzip

        stacked =
            values
                |> List.map (List.map (\e -> ( 0, e )))
                |> offset
    in
    { values = stacked
    , labels = labels
    , extent = calculateExtremes stacked
    }


{-| Calculates the minimal and maximal y values, for correct scaling
-}
calculateExtremes : List (List ( Float, Float )) -> ( Float, Float )
calculateExtremes coords =
    let
        folder ( y1, y2 ) ( accmin, accmax ) =
            ( Basics.min y1 y2 |> Basics.min accmin, Basics.max y1 y2 |> Basics.max accmax )
    in
    List.map (List.foldl folder ( 0, 0 )) coords
        |> List.foldl (\( mi, ma ) ( accmin, accmax ) -> ( Basics.min mi accmin, Basics.max ma accmax )) ( 0, 0 )


offsetNone : List (List ( Float, Float )) -> List (List ( Float, Float ))
offsetNone series =
    case series of
        [] ->
            []

        x :: xs ->
            let
                weirdAdd ( s10, s11 ) ( s00, s01 ) =
                    -- fall back to s00 when s01 is NaN
                    if isNaN s01 then
                        ( s00, s11 + s00 )

                    else
                        ( s01, s11 + s01 )

                helper s1 ( s0, accum ) =
                    ( List.map2 weirdAdd s1 s0
                    , s0 :: accum
                    )
            in
            List.foldl helper ( x, [] ) xs
                |> (\( a, b ) -> (::) a b)
                |> List.reverse


offsetDiverging :
    List (List ( number, number ))
    -> List (List ( number, number ))
offsetDiverging series =
    case series of
        [] ->
            []

        first :: rest ->
            let
                folder ( x, y ) ( yp, yn, accum ) =
                    let
                        dy =
                            y - x
                    in
                    if dy >= 0 then
                        ( yp + dy, yn, ( yp, yp + dy ) :: accum )

                    else if dy < 0 then
                        ( yp, yn + dy, ( yn + dy, yn ) :: accum )

                    else
                        ( yp, yn, ( yp, y ) :: accum )

                modifyColumn column =
                    List.foldl folder ( 0, 0, [] ) column
                        |> (\( _, _, newColumn ) -> newColumn)
                        |> List.reverse
            in
            series
                |> List.transpose
                |> List.map modifyColumn
                |> List.transpose


offsetExpand : List (List ( Float, Float )) -> List (List ( Float, Float ))
offsetExpand series =
    let
        -- divide each value in a column by the total sum of the column
        normalizeColumn column =
            let
                deltas =
                    List.map (abs << (\( a, b ) -> (-) a b)) column

                total =
                    List.sum deltas
            in
            List.map (\value -> ( 0, value / total )) deltas
    in
    series
        |> List.transpose
        |> List.map normalizeColumn
        |> List.transpose
        |> offsetNone


offsetSilhouette : List (List ( Float, Float )) -> List (List ( Float, Float ))
offsetSilhouette series =
    case series of
        [] ->
            []

        first :: xs ->
            let
                ys =
                    series
                        |> List.transpose
                        |> List.map (List.sum << List.map Tuple.second)
            in
            List.map2 (\( x, y ) newY -> ( -newY / 2, y + (-newY / 2) )) first ys
                :: xs
                |> offsetNone


offsetWiggle : List (List ( Float, Float )) -> List (List ( Float, Float ))
offsetWiggle series =
    case series of
        [] ->
            []

        first :: rest ->
            let
                columns : List (List Float)
                columns =
                    series
                        |> List.map (List.map Tuple.second)
                        |> List.transpose

                {- my best shot at describing what this does

                   * calculate delta - the difference between the current and the previous data point
                       this operates on the columns, so really it is the distance between g1(x) and g2(x).

                   * add that to sum, to get the current height of all processed layers

                   * add the height up to this point to half the height of the current layer,
                       multiply by the y-value of the current layer at the current point
                       add it to the accumulator

                -}
                folder ( prev, curr ) ( sum, accum ) =
                    let
                        delta =
                            curr - prev
                    in
                    ( delta + sum, (sum + (delta / 2)) * curr + accum )

                deltaFractions previous current =
                    -- deltas between the current and the previous column
                    List.map2 (\a b -> ( a, b )) previous current
                        |> List.foldl folder ( 0, 0 )
                        |> Tuple.second

                safeDivision a b =
                    if b == 0 then
                        0

                    else
                        a / b

                scanner ( columnSum, deltaFraction ) yValue =
                    yValue - safeDivision deltaFraction columnSum

                newFirst =
                    pairwise deltaFractions columns
                        |> List.map2 (\a b -> ( a, b )) (List.drop 1 columns |> List.map List.sum)
                        |> List.scanl scanner 0
                        |> List.map2 (\( x, y ) yValue -> ( yValue, y + yValue )) first
            in
            (newFirst :: rest)
                |> offsetNone


pairwise : (a -> a -> result) -> List a -> List result
pairwise f list =
    case list of
        [] ->
            []

        _ :: tail ->
            List.map2 f list tail


sortByInsideOut : (a -> Float) -> List a -> List a
sortByInsideOut toNumber items =
    -- NOTE this can't be (a -> number) in 0.18.
    -- because `(+)` needs a number and `List.sortBy` needs a `comparable`, using `a -> number` won't typecheck.
    -- This will be possible in 0.19
    let
        withSum =
            List.map (\element -> ( element, toNumber element )) items

        folder ( element, sum ) { bottom, bottoms, top, tops } =
            if top < bottom then
                { bottom = bottom, bottoms = bottoms, top = top + sum, tops = element :: tops }

            else
                { bottom = bottom + sum, bottoms = element :: bottoms, top = top, tops = tops }

        final =
            withSum
                |> List.sortBy Tuple.second
                |> List.foldl folder { bottom = 0, bottoms = [], top = 0, tops = [] }
    in
    List.reverse final.bottoms ++ final.tops

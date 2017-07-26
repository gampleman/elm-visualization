module Visualization.StackOffset
    exposing
        ( stackOffsetNone
        , stackOffsetDiverging
        , stackOffsetExpand
        , stackOffsetSilhouette
        , stackOffsetWiggle
        )

import List.Extra as List


stackOffsetNone : List (List ( Float, Float )) -> List (List ( Float, Float ))
stackOffsetNone series =
    case series of
        [] ->
            []

        x :: xs ->
            let
                weirdAdd ( s10, s11 ) ( s00, s01 ) =
                    -- no idea why this is what it is
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
                    |> uncurry (::)
                    |> List.reverse


stackOffsetDiverging series =
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


stackOffsetExpand series =
    case series of
        [] ->
            []

        first :: rest ->
            let
                transposed =
                    series
                        |> List.transpose

                ys =
                    transposed
                        |> List.map (List.sum << List.map Tuple.second)
            in
                List.map2 (\column newY -> List.map (\( x, y ) -> ( x, y / newY )) column) transposed ys
                    |> List.transpose
                    |> stackOffsetNone


stackOffsetSilhouette series =
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
                (List.map2 (\( x, y ) newY -> ( -newY / 2, y + (-newY / 2) )) first ys)
                    :: xs
                    |> stackOffsetNone


stackOffsetWiggle : List (List ( Float, Float )) -> List (List ( Float, Float ))
stackOffsetWiggle series =
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
                    List.map2 (,) previous current
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
                        |> List.map2 (,) (List.drop 1 columns |> List.map List.sum)
                        |> List.scanl scanner 0
                        |> List.map2 (\( x, y ) yValue -> ( yValue, y + yValue )) first
            in
                (newFirst :: rest)
                    |> stackOffsetNone


pairwise : (a -> a -> result) -> List a -> List result
pairwise f list =
    case list of
        [] ->
            []

        x :: xs ->
            List.map2 f (x :: xs) xs

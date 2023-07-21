module HierarchyTests exposing (..)

import Array
import Expect
import Fuzz exposing (Fuzzer)
import Hierarchy
import Hierarchy.Tree as Tree exposing (Tree)
import Test exposing (Test)


fuzzTree : Fuzzer child -> Fuzzer (Tree child)
fuzzTree child =
    let
        go depth branch =
            if depth > 0 then
                Fuzz.map2
                    (\label children ->
                        Tree.tree label children
                    )
                    child
                    (Fuzz.listOfLengthBetween 0 branch (go (depth - 1) branch))

            else
                Fuzz.map
                    (\label ->
                        Tree.singleton label
                    )
                    child
    in
    go 5 5


fuzzStratify : Fuzzer child -> Fuzzer (Tree child)
fuzzStratify child =
    Fuzz.list (Fuzz.listOfLengthBetween 1 32 (Fuzz.map2 Tuple.pair child (Fuzz.floatRange 0 1)))
        |> Fuzz.map
            (List.foldl
                (\row ( lst, prevN ) ->
                    ( List.map (\( val, p ) -> ( val, Just (floor (p * prevN)) )) row :: lst, toFloat (List.length row - 1) )
                )
                ( [], 0 )
                >> Tuple.first
                >> List.reverse
            )
        |> Fuzz.map (List.indexedMap (\rowIndex row -> List.indexedMap (\i ( val, parent ) -> { id = ( rowIndex + 1, i ), parentId = Maybe.map (\a -> ( rowIndex, a )) parent, value = val }) row))
        |> Fuzz.map2 (\root r -> { id = ( 0, 0 ), parentId = Nothing, value = root } :: List.concat r) child
        |> Fuzz.map2
            (\default r ->
                case Tree.stratify { id = .id, parentId = .parentId, transform = .value } r of
                    Ok t ->
                        t

                    Err _ ->
                        Tree.singleton default
            )
            child


fuzzBfsTree : Fuzzer child -> Fuzzer (Tree child)
fuzzBfsTree child =
    Fuzz.map2
        (\arr default ->
            let
                maxIdx =
                    Array.length arr - 1

                n =
                    Array.length arr |> toFloat

                normalized =
                    Array.map (Tuple.mapFirst (\a -> floor (a * n))) arr
            in
            Tree.unfold
                (\{ idx, totalNextLevel, offset } ->
                    let
                        ( numChildren, value ) =
                            Array.get idx normalized
                                |> Maybe.withDefault ( 0, default )

                        baseChildren =
                            List.range offset (min maxIdx (offset + numChildren - 1))

                        ( offsets, nextLevelChildrenCount ) =
                            baseChildren
                                |> List.map
                                    (\i ->
                                        Array.get i normalized |> Maybe.map Tuple.first |> Maybe.withDefault 0
                                    )
                                |> List.foldl (\num ( list, sum ) -> ( totalNextLevel + offset + sum :: list, num + sum )) ( [], 0 )
                                |> Tuple.mapFirst List.reverse
                    in
                    ( value
                    , List.map2 (\off i -> { idx = i, totalNextLevel = nextLevelChildrenCount, offset = off })
                        offsets
                        baseChildren
                    )
                )
                { idx = 0, totalNextLevel = Array.get 0 normalized |> Maybe.map Tuple.first |> Maybe.withDefault 0, offset = 1 }
        )
        (Fuzz.array (Fuzz.map2 Tuple.pair (Fuzz.floatRange 0 1) child))
        child

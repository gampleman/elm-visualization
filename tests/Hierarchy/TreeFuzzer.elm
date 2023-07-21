module Hierarchy.TreeFuzzer exposing (fuzzTree)

import Fuzz exposing (Fuzzer)
import Hierarchy
import Hierarchy.Tree as Tree exposing (Tree)
import Test exposing (Test)


fuzzTree : Fuzzer child -> Fuzzer (Tree child)
fuzzTree child =
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

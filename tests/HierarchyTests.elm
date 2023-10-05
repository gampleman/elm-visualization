module HierarchyTests exposing (fuzzTree)

import Fuzz exposing (Fuzzer)
import Tree exposing (Tree)


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

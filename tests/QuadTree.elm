module QuadTree exposing (insertTest)

import Test exposing (..)
import Expect
import Visualization.QuadTree as QuadTree exposing (QuadTree, Pointed)
import Fuzz exposing (..)
import Helper exposing (isAbout, isBetween, expectAll)


insertTest =
    describe "insert"
        [ test "increments length" <|
            \() ->
                QuadTree.empty
                    |> QuadTree.insert { x = 10, y = 10 }
                    |> QuadTree.length
                    |> Expect.equal 1

        -- , fuzz2 (quadtree point) point "increments the length" <|
        --     \tree val ->
        --         QuadTree.insert val tree
        --             |> QuadTree.length
        --             |> Expect.equal
        --                 (QuadTree.length tree + 1)
        ]


quadtree : Fuzzer (Pointed a) -> Fuzzer (QuadTree () (Pointed a))
quadtree =
    Fuzz.map (Debug.log "tree" << QuadTree.fromList << Debug.log "points") << list


point : Fuzzer (Pointed {})
point =
    let
        makePoint x y =
            { x = toFloat (floor x * 10) / 10
            , y = toFloat (floor y * 10) / 10
            }
    in
        Fuzz.map2 makePoint (floatRange 0.0 10.0) (floatRange 0.0 10.0)

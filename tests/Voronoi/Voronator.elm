module Voronoi.Voronator exposing (voronatorTest)

import Array.Hamt as Array
import Expect
import IntDict
import Test exposing (..)
import Visualization.Voronoi.Delaunator exposing (delaunator)
import Visualization.Voronoi.Voronator exposing (voronoi)


crashIfBad a =
    case a of
        Ok v ->
            v

        Err s ->
            Debug.crash s


voronatorTest : Test
voronatorTest =
    describe "voronator"
        [ describe "returns the expected diagram"
            (let
                diagram =
                    [ 0, 0, 1, 0, 0, 1, 1, 1 ]
                        |> Array.fromList
                        |> delaunator
                        |> crashIfBad
                        |> voronoi
             in
                [ test "matches edges" <|
                    \() ->
                        diagram.edges
                            |> Array.toList
                            |> Expect.equalLists
                                [ 0, 0, 0 ]
                ]
            )

        -- |> Expect.equal
        --     { points = Array.fromList [ 0, 0, 1, 0, 0, 1, 1, 1 ]
        --     , edges = IntDict.fromList [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ) ]
        --     , circumcenters = Array.fromList [ 0.5, 0.5, 0.5, 0.5 ]
        --     , index = IntDict.fromList [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ), ( 3, 3 ), ( 4, 1 ), ( 5, 2 ), ( 6, 0 ), ( 7, 0 ) ]
        --     , vectors = IntDict.fromList [ ( 0, 0 ), ( 1, -1 ), ( 2, -1 ), ( 3, 0 ), ( 4, 1 ), ( 5, 1 ), ( 6, 0 ), ( 7, -1 ), ( 8, -1 ), ( 9, 0 ), ( 10, 1 ), ( 11, 1 ), ( 12, 0 ), ( 13, 0 ), ( 14, 0 ), ( 15, 0 ) ]
        --     }
        ]

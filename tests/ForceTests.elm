module ForceTests exposing (entityTest, integrationTest)

import Array
import Expect
import Force
import Test exposing (..)


entityTest : Test
entityTest =
    describe "entity"
        [ test "computes a petal like layout based on indexes" <|
            \() ->
                [ (), (), () ]
                    |> List.indexedMap Force.entity
                    |> Expect.equal
                        [ { x = 0
                          , y = 0
                          , vx = 0
                          , vy = 0
                          , value = ()
                          , id = 0
                          }
                        , { x = 10 * cos (pi * (3 - sqrt 5))
                          , y = 10 * sin (pi * (3 - sqrt 5))
                          , vx = 0
                          , vy = 0
                          , value = ()
                          , id = 1
                          }
                        , { x = sqrt 2 * 10 * cos (2 * pi * (3 - sqrt 5))
                          , y = sqrt 2 * 10 * sin (2 * pi * (3 - sqrt 5))
                          , vx = 0
                          , vy = 0
                          , value = ()
                          , id = 2
                          }
                        ]
        ]


integrationTest : Test
integrationTest =
    test "computeSimulation with a few forces computes a triangle" <|
        \() ->
            let
                entities =
                    List.indexedMap Force.entity [ (), (), () ]

                forces =
                    [ Force.manyBody <| List.map .id entities
                    , Force.links [ ( 0, 1 ), ( 1, 2 ), ( 2, 0 ) ]
                    , Force.center 0.0 0.0
                    ]

                simulation =
                    Force.simulation forces

                forceMaybe m =
                    case m of
                        Just x ->
                            x

                        Nothing ->
                            Force.entity 0 ()

                dist n1 n2 =
                    sqrt ((n1.x - n2.x) ^ 2 + (n1.y - n2.y) ^ 2)

                computeDistances array =
                    let
                        n0 =
                            forceMaybe <| Array.get 0 array

                        n1 =
                            forceMaybe <| Array.get 1 array

                        n2 =
                            forceMaybe <| Array.get 2 array
                    in
                    [ dist n0 n1, dist n1 n2, dist n2 n0 ]
            in
            Force.computeSimulation simulation entities
                |> Array.fromList
                |> computeDistances
                |> List.map round
                |> Expect.equal [ 34, 34, 34 ]

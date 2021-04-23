module ForceTests exposing (collisionTest, entityTest, integrationTest)

import Array
import Expect
import Force
import Test exposing (Test, describe, test)


runTicks iterations simulation entitites =
    if iterations > 0 then
        let
            ( newSim, newEnts ) =
                Force.tick simulation entitites
        in
        runTicks (iterations - 1) newSim newEnts

    else
        entitites


collisionTest : Test
collisionTest =
    describe "collision"
        [ test "colides nodes" <|
            \() ->
                let
                    nodes =
                        [ (), (), () ]
                            |> List.indexedMap Force.entity

                    simulation =
                        Force.simulation [ Force.collision 100 [ 0, 1, 2 ] ]
                in
                runTicks 10 simulation nodes
                    |> Expect.equal
                        [ { id = 0, value = (), vx = 0.677346615710878, vy = 0.26976816231064366, x = 174.08616723117228, y = 66.51743051995629 }
                        , { id = 1, value = (), vx = -0.6981367135383595, vy = 0.7334733779701496, x = -181.17225489926076, y = 189.1274524695976 }
                        , { id = 2, value = (), vx = 0.020790097827481858, vy = -1.0032415402807933, x = 6.50859004343442, y = -263.1226973950056 }
                        ]

        -- [ { id = 0, x = 174.08616723117228, y = 66.51743051995625, vy = 0.26976816231064354, vx = 0.677346615710878, value = () }
        -- , { id = 1, x = -139.73606544743998, y = 95.69860503079263, vy = 0.3545632444404687, vx = -0.5300880593105067, value = () }
        -- , { id = 2, x = -34.9275994083864, y = -169.69384995620052, vy = -0.6243314067511122, vx = -0.1472585564003713, value = () }
        -- ]
        ]


entityTest : Test
entityTest =
    describe "entity"
        [ test "computes a petal like layout based on indexes" <|
            \() ->
                [ (), (), () ]
                    |> List.indexedMap Force.entity
                    |> Expect.equal
                        [ { x = sqrt 0.5 * 10 * cos 0
                          , y = sqrt 0.5 * 10 * sin 0
                          , vx = 0
                          , vy = 0
                          , value = ()
                          , id = 0
                          }
                        , { x = sqrt 1.5 * 10 * cos (pi * (3 - sqrt 5))
                          , y = sqrt 1.5 * 10 * sin (pi * (3 - sqrt 5))
                          , vx = 0
                          , vy = 0
                          , value = ()
                          , id = 1
                          }
                        , { x = sqrt 2.5 * 10 * cos (2 * pi * (3 - sqrt 5))
                          , y = sqrt 2.5 * 10 * sin (2 * pi * (3 - sqrt 5))
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

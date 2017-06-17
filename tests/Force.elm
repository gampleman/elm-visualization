module Force exposing (entityTest)

import Expect
import Test exposing (..)
import Visualization.Force as Force


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

module Scale.DivergingTests exposing (diverging, divergingLog)

import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (..)
import Helper exposing (expectAll, isAbout, isBetween)
import Interpolation
import Scale
import Test exposing (..)


convert val scale =
    Scale.convert scale val


diverging : Test
diverging =
    describe "Scale.diverging"
        [ fuzz float "default-ish values work like identity" <|
            \s ->
                Scale.convert (Scale.diverging identity ( 0, 0.5, 1 )) s
                    |> Expect.within (Absolute 0.00001) s
        , test "works with a custom zero center domain" <|
            \() ->
                Scale.diverging identity ( -1.2, 0, 2.4 )
                    |> Expect.all
                        [ convert -1.2 >> Expect.within (Absolute 0.00001) 0
                        , convert 0.6 >> Expect.within (Absolute 0.00001) 0.625
                        , convert 2.4 >> Expect.within (Absolute 0.00001) 1
                        ]
        , test "handles a degenerate domain x0 = x1" <|
            \() ->
                Scale.diverging identity ( 2, 2, 3 )
                    |> Expect.all
                        [ convert -1.2 >> Expect.within (Absolute 0.00001) 0.5
                        , convert 0.6 >> Expect.within (Absolute 0.00001) 0.5
                        , convert 2.4 >> Expect.within (Absolute 0.00001) 0.7
                        ]
        , test "handles a degenerate domain x1 = x2" <|
            \() ->
                Scale.diverging identity ( 1, 2, 2 )
                    |> Expect.all
                        [ convert -1 >> Expect.within (Absolute 0.00001) -1
                        , convert 0.5 >> Expect.within (Absolute 0.00001) -0.25
                        , convert 2.4 >> Expect.within (Absolute 0.00001) 0.5
                        ]
        , test "handles a degenerate domain x0 = x1 = x2" <|
            \() ->
                Scale.diverging identity ( 2, 2, 2 )
                    |> Expect.all
                        [ convert -1 >> Expect.within (Absolute 0.00001) 0.5
                        , convert 0.5 >> Expect.within (Absolute 0.00001) 0.5
                        , convert 2.4 >> Expect.within (Absolute 0.00001) 0.5
                        ]
        , test "handles a descending domain" <|
            \() ->
                Scale.diverging identity ( 4, 2, 1 )
                    |> Expect.all
                        [ convert 1.2 >> Expect.within (Absolute 0.00001) 0.9
                        , convert 2.0 >> Expect.within (Absolute 0.00001) 0.5
                        , convert 3.0 >> Expect.within (Absolute 0.00001) 0.25
                        ]
        , test "works with a different interpolator" <|
            \() ->
                Scale.diverging (\t -> t * 2) ( 0, 0.5, 1 )
                    |> Expect.all
                        [ convert -0.5 >> Expect.within (Absolute 0.00001) -1
                        , convert 0 >> Expect.within (Absolute 0.00001) 0
                        , convert 0.5 >> Expect.within (Absolute 0.00001) 1
                        ]
        ]


divergingLog =
    describe "Scale.divergingLog"
        [ test "handles a descending domain" <|
            \() ->
                Scale.divergingLog e identity ( 3, 2, 1 )
                    |> Expect.all
                        [ convert 1.2 >> Expect.within (Absolute 0.00001) (1 - 0.1315172029168969)
                        , convert 2.0 >> Expect.within (Absolute 0.00001) (1 - 0.5)
                        , convert 2.8 >> Expect.within (Absolute 0.00001) (1 - 0.9149213210862197)
                        ]
        , test "handles a descending negative domain" <|
            \() ->
                Scale.divergingLog e identity ( -1, -2, -3 )
                    |> Expect.all
                        [ convert -1.2 >> Expect.within (Absolute 0.00001) 0.1315172029168969
                        , convert -2.0 >> Expect.within (Absolute 0.00001) 0.5
                        , convert -2.8 >> Expect.within (Absolute 0.00001) 0.9149213210862197
                        ]
        ]

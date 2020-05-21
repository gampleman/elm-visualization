module Scale.ThresholdTests exposing (all)

import Expect
import Fuzz exposing (..)
import Helper exposing (expectAll, isAbout, isBetween)
import Scale
import Test exposing (..)


type Data
    = A
    | B
    | C


convert val scale =
    Scale.convert scale val


all : Test
all =
    describe "Scale.threshold"
        [ test "maps a number to a discrete value in the range" <|
            \() ->
                Scale.threshold ( A, [ ( 1 / 3, B ), ( 2 / 3, C ) ] )
                    |> Expect.all
                        [ convert 0.0 >> Expect.equal A
                        , convert 0.2 >> Expect.equal A
                        , convert 0.4 >> Expect.equal B
                        , convert 0.6 >> Expect.equal B
                        , convert 0.8 >> Expect.equal C
                        , convert 1.0 >> Expect.equal C
                        ]
        , test "supports arbitrary orderable ranges" <|
            \() ->
                Scale.threshold ( 0, [ ( "10", 1 ), ( "2", 2 ) ] )
                    |> Expect.all
                        [ convert "0" >> Expect.equal 0
                        , convert "12" >> Expect.equal 1
                        , convert "3" >> Expect.equal 2
                        ]
        ]

module Scale.BandTests exposing (band)

import Expect
import Helper exposing (assert, expectAll)
import Scale exposing (defaultBandConfig)
import Test exposing (Test, describe, test)


band : Test
band =
    describe "band"
        [ test "convert a value in the domain returns a a nicely computed value in the range" <|
            \() ->
                let
                    scale =
                        Scale.band defaultBandConfig ( 0, 960 ) [ "foo", "bar" ]
                in
                expectAll
                    [ Scale.convert scale "foo"
                        |> Expect.equal 0
                    , Scale.convert scale "bar"
                        |> Expect.equal 480
                    ]
        , test "returns NaN for values outside the domain" <|
            \() ->
                let
                    scale =
                        Scale.band defaultBandConfig ( 0, 960 ) [ "foo", "bar" ]
                in
                expectAll
                    [ Scale.convert scale "baz"
                        |> isNaN
                        >> assert "isNan"
                    ]
        , test "range values can be descending" <|
            \() ->
                let
                    domain =
                        [ "a", "b", "c" ]

                    scale =
                        Scale.band defaultBandConfig ( 120, 0 ) domain
                in
                expectAll
                    [ domain
                        |> List.map (Scale.convert scale)
                        |> Expect.equal [ 80, 40, 0 ]
                    , Scale.bandwidth scale
                        |> Expect.equal 40
                    ]
        ]

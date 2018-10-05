module Scale.BandTests exposing (convert)

import Expect
import Fuzz exposing (..)
import Helper exposing (expectAll)
import Scale exposing (defaultBandConfig)
import Svg.Attributes exposing (scale)
import Test exposing (..)


elementInList : Fuzzer a -> Fuzzer ( a, List a )
elementInList fuzzer =
    Fuzz.map3 (\prefix item postfix -> ( item, List.concat [ prefix, [ item ], postfix ] )) (list fuzzer) fuzzer (list fuzzer)


nonEmptyList : Fuzzer a -> Fuzzer (List a)
nonEmptyList fuzzer =
    Fuzz.map2 (\head tail -> head :: tail) fuzzer (list fuzzer)


convert : Test
convert =
    describe "convert"
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
                        >> Expect.true "isNan"
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

module Scale.Ordinal exposing (convert)

import Expect
import Fuzz exposing (..)
import Helper exposing (expectMember)
import Test exposing (..)
import Visualization.Scale as Scale


elementInList : Fuzzer a -> Fuzzer ( a, List a )
elementInList fuzzer =
    Fuzz.map3 (\prefix item postfix -> ( item, List.concat [ prefix, [ item ], postfix ] )) (list fuzzer) fuzzer (list fuzzer)


nonEmptyList : Fuzzer a -> Fuzzer (List a)
nonEmptyList fuzzer =
    Fuzz.map2 (\head tail -> head :: tail) fuzzer (list fuzzer)


convert : Test
convert =
    describe "convert"
        [ test "convert a value in the domain returns a corresponding value in the range" <|
            \() ->
                let
                    scale =
                        Scale.ordinal [ 1, 2, 3 ] [ "a", "b" ]
                in
                    Scale.convert scale 1
                        |> Expect.equal (Just "a")
        , test "convert a value in the domain returns a corresponding value in a wrapped range" <|
            \() ->
                let
                    scale =
                        Scale.ordinal [ 1, 2, 3 ] [ "a", "b" ]
                in
                    Scale.convert scale 3
                        |> Expect.equal (Just "a")
        , fuzz2 (elementInList int) (nonEmptyList string) "converted value is a valid domain" <|
            \( value, domain ) range ->
                let
                    scale =
                        Scale.ordinal domain range
                in
                    Scale.convert scale value
                        |> Maybe.map (expectMember range)
                        |> Maybe.withDefault (Expect.fail "was nothing")
        , fuzz2 (list int) int "if range is empty returns Nothing" <|
            \domain value ->
                let
                    scale =
                        Scale.ordinal domain []
                in
                    Scale.convert scale value
                        |> Expect.equal Nothing
        ]

module Scale.LogTests exposing (clampTest, convertTest, rangeExtentTest, tickFormatTest, ticksTest)

import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (float, floatRange, pair, triple)
import Helper exposing (isBetween)
import Scale
import Test exposing (Test, describe, fuzz, test)


convertsTo : Float -> Float -> Test
convertsTo input expected =
    test ("converts " ++ String.fromFloat input ++ " to " ++ String.fromFloat expected) <|
        \() ->
            Scale.convert (Scale.log 10 ( 0, 1 ) ( 1, 2 )) input
                |> Expect.within (Absolute 0.00001) expected


convertTest : Test
convertTest =
    describe "convert"
        [ convertsTo 0.5 -1.0
        , convertsTo 1.0 0.0
        , convertsTo 1.5 0.5849625
        , convertsTo 2.0 1.0
        , convertsTo 2.5 1.3219281
        ]


empty : ( Float, Float ) -> Bool
empty ( a, b ) =
    a == b


wiggeIfZero : Float -> Float
wiggeIfZero a =
    if a == 0 then
        0.01

    else
        a


normalizeDomain : ( Float, Float ) -> ( Float, Float )
normalizeDomain ( mn, mx ) =
    if mn < 0 then
        ( wiggeIfZero mn, -1 * wiggeIfZero (abs mx) )

    else
        ( wiggeIfZero mn, abs (wiggeIfZero mx) )


rangePair =
    Fuzz.map3 (\a b c -> ( a, a + c * b )) (Fuzz.floatRange -1000 1000) (Fuzz.floatRange 0.1 1000) (Fuzz.oneOf [ Fuzz.constant -1, Fuzz.constant 1 ])


clampTest : Test
clampTest =
    fuzz (triple rangePair rangePair (pair (floatRange 1 20) Fuzz.niceFloat)) "clamp limits output value to the range" <|
        \( domain, range, ( base, val ) ) ->
            if empty domain || empty range then
                Expect.pass

            else
                let
                    convert =
                        Scale.convert (Scale.clamp (Scale.log base range (normalizeDomain domain))) val
                in
                convert |> isBetween range


rangeExtentTest : Test
rangeExtentTest =
    fuzz (triple (pair float float) (pair float float) (pair float float)) "rangeExtent returns the range" <|
        \( domain, range, ( base, _ ) ) ->
            Scale.rangeExtent (Scale.log base range domain) |> Expect.equal range


powerOfTenTicks : String -> ( Float, Float ) -> List Float -> Test
powerOfTenTicks desc =
    powerOfTenTicksCount desc 10


powerOfTenTicksCount : String -> Int -> ( Float, Float ) -> List Float -> Test
powerOfTenTicksCount desc count ( begin, end ) expected =
    let
        description =
            "generates the expected power-of-ten for " ++ desc ++ " domains when base = 10 for domain = ( " ++ String.fromFloat begin ++ ", " ++ String.fromFloat end ++ " ) and count = " ++ String.fromInt count
    in
    test description <|
        \() ->
            let
                scale =
                    Scale.log 10 ( 0, 1 ) ( begin, end )
            in
            Scale.ticks scale 10
                |> List.map (\v -> toFloat (round (v * 1.0e12)) / 1.0e12)
                |> Expect.equal expected


ticksTest : Test
ticksTest =
    describe "ticks"
        [ powerOfTenTicks "ascending" ( 1.0e-1, 1.0e1 ) [ 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
        , powerOfTenTicks "ascending" ( 1.0e-1, 1.0e0 ) [ 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1 ]
        , powerOfTenTicks "ascending" ( -1.0e0, -1.0e-1 ) [ -1, -0.9, -0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1 ]
        , powerOfTenTicks "desceding" ( -1.0e-1, -1.0e1 ) [ -0.1, -0.2, -0.3, -0.4, -0.5, -0.6, -0.7, -0.8, -0.9, -1, -2, -3, -4, -5, -6, -7, -8, -9, -10 ]
        , powerOfTenTicks "descending" ( -1.0e-1, -1.0e0 ) [ -0.1, -0.2, -0.3, -0.4, -0.5, -0.6, -0.7, -0.8, -0.9, -1 ]
        , powerOfTenTicks "descending" ( 1.0e0, 1.0e-1 ) [ 1, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1 ]
        , powerOfTenTicks "small" ( 1, 5 ) [ 1, 2, 3, 4, 5 ]
        , powerOfTenTicks "small" ( 5, 1 ) [ 5, 4, 3, 2, 1 ]
        , powerOfTenTicks "small" ( -1, -5 ) [ -1, -2, -3, -4, -5 ]
        , powerOfTenTicks "small" ( -5, -1 ) [ -5, -4, -3, -2, -1 ]

        -- , powerOfTenTicksCount "small" 1 ( 286.9252014, 329.4978332 ) [ 300 ]
        -- , powerOfTenTicksCount "small" 2 ( 286.9252014, 329.4978332 ) [ 300 ]
        -- , powerOfTenTicksCount "small" 3 ( 286.9252014, 329.4978332 ) [ 300, 320 ]
        -- , powerOfTenTicksCount "small" 4 ( 286.9252014, 329.4978332 ) [ 290, 300, 310, 320 ]
        -- , powerOfTenTicks "small" ( 286.9252014, 329.4978332 ) [ 290, 295, 300, 305, 310, 315, 320, 325 ]
        , test "generates the expected power-of-base ticks" <|
            \() ->
                let
                    scale =
                        Scale.log e ( 0, 1 ) ( 0.1, 100 )
                in
                Scale.ticks scale 10
                    |> List.map (\v -> toFloat (round (v * 1.0e12)) / 1.0e12)
                    |> Expect.equal [ 0.135335283237, 0.367879441171, 1, 2.718281828459, 7.389056098931, 20.085536923188, 54.598150033144 ]
        ]


baseFormat : Float -> Int -> List String -> Test
baseFormat base count expected =
    test ("formats ticks nicely with base=" ++ String.fromFloat base ++ " count=" ++ String.fromInt count) <|
        \() ->
            let
                scale =
                    Scale.log base ( 0, 1 ) ( 1.0e-1, 1.0e1 )
            in
            Scale.ticks scale 10
                |> List.map (Scale.tickFormat scale count)
                |> Expect.equal expected


baseTenFormat : Int -> List String -> Test
baseTenFormat =
    baseFormat 10


tickFormatTest : Test
tickFormatTest =
    describe "tickFormat"
        [ baseTenFormat 10 [ "1e-1", "2e-1", "3e-1", "4e-1", "5e-1", "", "", "", "", "1e+0", "2e+0", "3e+0", "4e+0", "5e+0", "", "", "", "", "1e+1" ]
        , baseTenFormat 5 [ "1e-1", "2e-1", "", "", "", "", "", "", "", "1e+0", "2e+0", "", "", "", "", "", "", "", "1e+1" ]
        , baseTenFormat 1 [ "1e-1", "", "", "", "", "", "", "", "", "1e+0", "", "", "", "", "", "", "", "", "1e+1" ]
        , baseTenFormat 0 [ "1e-1", "", "", "", "", "", "", "", "", "1e+0", "", "", "", "", "", "", "", "", "1e+1" ]
        , baseFormat e 10 [ "0.135335283237", "0.367879441171", "1", "2.71828182846", "7.38905609893" ]
        , baseFormat 16 10 [ "0.125", "0.1875", "0.25", "0.3125", "0.375", "", "", "", "", "", "", "", "", "", "1", "2", "3", "4", "5", "6", "", "", "", "" ]
        , baseFormat 16 5 [ "0.125", "0.1875", "", "", "", "", "", "", "", "", "", "", "", "", "1", "2", "3", "", "", "", "", "", "", "" ]
        , baseFormat 16 1 [ "", "", "", "", "", "", "", "", "", "", "", "", "", "", "1", "", "", "", "", "", "", "", "", "" ]
        ]

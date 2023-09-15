module Color.LabTests exposing (suite)

import Color exposing (Color)
import Color.Lab as Lab
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, floatRange, triple)
import Test exposing (Test, describe, fuzz, fuzz2, test)


unit : Fuzzer Float
unit =
    floatRange 0 1


guaranteedTolerance : FloatingPointTolerance
guaranteedTolerance =
    Absolute 0.0000000001


uint8 : Fuzzer Int
uint8 =
    Fuzz.intRange 0 255


toColorFloat : Int -> Float
toColorFloat n =
    toFloat n / 255


color : Fuzzer Color
color =
    Fuzz.map4 (\r g b a -> Color.rgba (toColorFloat r) (toColorFloat g) (toColorFloat b) a) uint8 uint8 uint8 unit


suite : Test
suite =
    describe "Color"
        [ describe "Color lab"
            [ fuzz2 (triple (floatRange 0 100) (floatRange -160 160) (floatRange -160 160)) unit "can represent Lab colors (fromLab)" <|
                \( l, a, b ) alpha ->
                    Lab.fromLab { l = l, a = a, b = b, alpha = alpha }
                        |> Lab.toLab
                        |> Expect.all
                            [ .l >> Expect.within (Absolute 0.001) l
                            , .a >> Expect.within (Absolute 0.001) a
                            , .b >> Expect.within (Absolute 0.001) b
                            , .alpha >> Expect.within guaranteedTolerance alpha
                            ]
            , test "hcl exposes the hue, chroma, luminance values" <|
                \() ->
                    Color.rgb255 170 187 204
                        |> Lab.toHcl
                        |> expectHclEqual { hue = 252.37145234745182, chroma = 11.223567114593477, luminance = 74.96879980931759, alpha = 1 }
            , test "hcl converts to proper RGB" <|
                \() ->
                    Lab.fromHcl { hue = 120, chroma = 30, luminance = 50, alpha = 0.4 }
                        |> Color.toRgba
                        |> expectRgbEqual { red = 105 / 255, green = 126 / 255, blue = 73 / 255, alpha = 0.4 }
            , fuzz color "can represent Hcl colors (fromHcl)" <|
                \col ->
                    col
                        |> Lab.toHcl
                        |> Lab.fromHcl
                        |> Color.toRgba
                        |> expectRgbEqual (Color.toRgba col)
            ]
        ]


expectHclEqual : { a | hue : Float, chroma : Float, luminance : Float, alpha : d } -> { b | hue : Float, chroma : Float, luminance : Float, alpha : d } -> Expectation
expectHclEqual expected actual =
    if
        areHclCoordsEqual actual.hue expected.hue
            && areHclCoordsEqual actual.chroma expected.chroma
            && areHclCoordsEqual actual.luminance expected.luminance
            && expected.alpha
            == actual.alpha
    then
        Expect.pass

    else
        Expect.fail (Debug.toString expected ++ "\n    ╷\n    │ expectHclEqual\n    ╵\n" ++ Debug.toString actual)


expectRgbEqual : { a | red : Float, green : Float, blue : Float, alpha : d } -> { b | red : Float, green : Float, blue : Float, alpha : d } -> Expectation
expectRgbEqual expected actual =
    if
        areRgbCoordsEqual actual.red expected.red
            && areRgbCoordsEqual actual.green expected.green
            && areRgbCoordsEqual actual.blue expected.blue
            && expected.alpha
            == actual.alpha
    then
        Expect.pass

    else
        Expect.fail (Debug.toString expected ++ "\n    ╷\n    │ expectRgbEqual\n    ╵\n" ++ Debug.toString actual)


areHclCoordsEqual : Float -> Float -> Bool
areHclCoordsEqual expected actual =
    (isNaN actual && isNaN expected) || (expected - 1.0e-6 <= actual && actual <= expected + 1.0e-6)


areRgbCoordsEqual : Float -> Float -> Bool
areRgbCoordsEqual expected actual =
    round (expected * 255) == round (actual * 255)

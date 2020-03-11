module Color.LabTests exposing (suite)

import Array
import Color exposing (Color)
import Color.Lab as Lab
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, float, floatRange, intRange, list)
import Test exposing (..)


unit : Fuzzer Float
unit =
    floatRange 0 1


guaranteedTolerance =
    Absolute 0.0000000001


tuple2 : Fuzzer a -> Fuzzer b -> Fuzzer ( a, b )
tuple2 a b =
    Fuzz.tuple ( a, b )


tuple3 : Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer ( a, b, c )
tuple3 a b c =
    Fuzz.tuple3 ( a, b, c )


suite : Test
suite =
    describe "Color"
        [ describe "Color lab"
            [ fuzz (tuple2 (tuple3 (floatRange 0 100) (floatRange -160 160) (floatRange -160 160)) unit)
                "can represent Lab colors (fromLab)"
              <|
                \( ( l, a, b ), alpha ) ->
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
            , fuzz (tuple2 (tuple3 unit unit unit) unit)
                "can represent Hcl colors (fromHcl)"
              <|
                \( ( r, g, b ), alpha ) ->
                    Color.rgba r g b alpha
                        |> Lab.toHcl
                        |> Lab.fromHcl
                        |> Color.toRgba
                        |> expectRgbEqual { red = r, green = g, blue = b, alpha = alpha }
            ]
        ]


expectHclEqual expected actual =
    Expect.true (Debug.toString expected ++ "\n    ╷\n    │ expectHclEqual\n    ╵\n" ++ Debug.toString actual) <|
        areHclCoordsEqual actual.hue expected.hue
            && areHclCoordsEqual actual.chroma expected.chroma
            && areHclCoordsEqual actual.luminance expected.luminance
            && expected.alpha
            == actual.alpha


expectRgbEqual expected actual =
    Expect.true (Debug.toString expected ++ "\n    ╷\n    │ expectRgbEqual\n    ╵\n" ++ Debug.toString actual) <|
        areRgbCoordsEqual actual.red expected.red
            && areRgbCoordsEqual actual.green expected.green
            && areRgbCoordsEqual actual.blue expected.blue
            && expected.alpha
            == actual.alpha


areHclCoordsEqual expected actual =
    (isNaN actual && isNaN expected) || (expected - 1.0e-6 <= actual && actual <= expected + 1.0e-6)


areRgbCoordsEqual expected actual =
    round (expected * 255) == round (actual * 255)

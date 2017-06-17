module Axis exposing (..)

import Test exposing (..)
import Expect
import Visualization.Axis as Axis
import Visualization.Scale as Scale
import Fuzz exposing (..)
import Helper exposing (isAbout, isBetween, expectAll)
import Svg exposing (g, path, line, text, text_)
import Svg.Attributes exposing (class, stroke, fill, transform, d, fontSize, fontFamily, x, y, x1, x2, y1, y2, dy, textAnchor)


defaultOptionsTest : Test
defaultOptionsTest =
    test "defaultOptions returns sensible defaults" <|
        \() ->
            Axis.defaultOptions
                |> Expect.equal
                    { orientation = Axis.Left
                    , ticks = Nothing
                    , tickFormat = Nothing
                    , tickCount = 10
                    , tickSizeInner = 6
                    , tickSizeOuter = 6
                    , tickPadding = 3
                    }


renderingTest : Test
renderingTest =
    test "it has a stable rendering" <|
        \() ->
            let
                expected =
                    g [ fill "none", fontSize "10", fontFamily "sans-serif", textAnchor "end" ]
                        [ path [ class "domain", stroke "#000", d "M-6,290.5H0.5V0.5H-6" ] []
                        , g [ class "tick", transform "translate(0, 290)" ]
                            [ line [ stroke "#000", x2 "-6", y1 "0.5", y2 "0.5" ] []
                            , text_ [ fill "#000", x "-9", y "0.5", dy "0.32em" ] [ text "0" ]
                            ]
                        , g [ class "tick", transform "translate(0, 232)" ]
                            [ line [ stroke "#000", x2 "-6", y1 "0.5", y2 "0.5" ] []
                            , text_ [ fill "#000", x "-9", y "0.5", dy "0.32em" ] [ text "1" ]
                            ]
                        , g [ class "tick", transform "translate(0, 174)" ]
                            [ line [ stroke "#000", x2 "-6", y1 "0.5", y2 "0.5" ] []
                            , text_ [ fill "#000", x "-9", y "0.5", dy "0.32em" ] [ text "2" ]
                            ]
                        , g [ class "tick", transform "translate(0, 116)" ]
                            [ line [ stroke "#000", x2 "-6", y1 "0.5", y2 "0.5" ] []
                            , text_ [ fill "#000", x "-9", y "0.5", dy "0.32em" ] [ text "3" ]
                            ]
                        , g [ class "tick", transform "translate(0, 58)" ]
                            [ line [ stroke "#000", x2 "-6", y1 "0.5", y2 "0.5" ] []
                            , text_ [ fill "#000", x "-9", y "0.5", dy "0.32em" ] [ text "4" ]
                            ]
                        , g [ class "tick", transform "translate(0, 0)" ]
                            [ line [ stroke "#000", x2 "-6", y1 "0.5", y2 "0.5" ] []
                            , text_ [ fill "#000", x "-9", y "0.5", dy "0.32em" ] [ text "5" ]
                            ]
                        ]

                options =
                    Axis.defaultOptions
            in
                Axis.axis { options | tickCount = 5 } (Scale.linear ( 0, 5 ) ( 290, 0 ))
                    |> Expect.equal expected

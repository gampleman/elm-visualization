module AxisTests exposing (renderingTest)

import Axis
import Expect
import Fuzz exposing (..)
import Helper exposing (expectAll, isAbout, isBetween)
import Scale
import Svg exposing (g, line, path, text, text_)
import Svg.Attributes exposing (class, d, dy, fill, fontFamily, fontSize, stroke, textAnchor, transform, x, x1, x2, y, y1, y2)
import Test exposing (..)


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
            in
            Axis.left [ Axis.tickCount 5 ] (Scale.linear ( 290, 0 ) ( 0, 5 ))
                |> Expect.equal expected

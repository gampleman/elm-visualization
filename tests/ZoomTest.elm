module ZoomTest exposing (interpolateTest)

import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (..)
import Helper exposing (expectAll, expectAny)
import Test exposing (..)
import Zoom.Interpolation exposing (interpolate)


interpolateTest : Test
interpolateTest =
    test "interpolateZoom(a, b) handles nearly-coincident points" <|
        \() ->
            let
                a =
                    { cx = 324.68721096803614, cy = 59.43501602433761, size = 1.8827137399562621 }

                b =
                    { cx = 324.6872108946794, cy = 59.43501601062763, size = 7.399052110984391 }

                ( t, inter ) =
                    interpolate a b
            in
            inter 0.5
                |> Expect.equal { cx = 324.68721093135775, cy = 59.43501601748262, size = 3.7323313186268305 }

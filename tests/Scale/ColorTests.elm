module Scale.ColorTests exposing (all)

import Color exposing (Color, rgb255, toRgba)
import Expect
import Helper exposing (expectAll, isAbout, isBetween)
import Scale.Color
import Test exposing (..)


all : Test
all =
    describe "Scale.Color.cielabInterpolator"
        [ only <|
            test "CIELAB color space interpolator" <|
                \() ->
                    let
                        red =
                            rgb255 255 0 0

                        blue =
                            rgb255 0 0 255

                        interpolator =
                            Scale.Color.cielabInterpolator ( red, blue )
                    in
                    expectAll
                        [ interpolator 0 |> Expect.equal red

                        --interpolator 0.5 |> Debug.log "space" |> Expect.equal (rgb255 245 0 134)
                        , interpolator 1 |> Expect.equal blue
                        ]
        ]

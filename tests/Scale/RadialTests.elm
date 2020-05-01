module Scale.RadialTests exposing (all)

import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (..)
import Helper exposing (expectAll, isAbout, isBetween)
import Scale
import Test exposing (..)


nonZero =
    Fuzz.oneOf
        [ Fuzz.floatRange 0.0001 20
        , Fuzz.floatRange -20 -0.0001
        ]


all : Test
all =
    describe "Scale.radial"
        [ test "convert maps a domain value x to range value y" <|
            \() ->
                Scale.convert (Scale.radial ( 1, 2 ) ( 0, 1 )) 0.5
                    |> Expect.within (Absolute 0.0001) 1.5811388300841898
        , test "maps an empty domain to the middle of the range" <|
            \() ->
                expectAll
                    [ Scale.convert (Scale.radial ( 1, 2 ) ( 0, 0 )) 0
                        |> Expect.within (Absolute 0.0001) 1.5811388300841898
                    , Scale.convert (Scale.radial ( 2, 1 ) ( 0, 0 )) 1
                        |> Expect.within (Absolute 0.0001) 1.5811388300841898
                    ]
        , test "invert maps a range value y to a domain value x" <|
            \() ->
                Scale.invert (Scale.radial ( 1, 2 ) ( 0, 1 )) 1.5
                    |> Expect.within (Absolute 0.00001) 0.4166666666666667
        , test "invert y maps an empty range to the middle of the domain" <|
            \() ->
                expectAll
                    [ Scale.invert (Scale.radial ( 0, 0 ) ( 1, 2 )) 0
                        |> Expect.within (Absolute 0.0001) 1.5
                    , Scale.invert (Scale.radial ( 0, 0 ) ( 2, 1 )) 1
                        |> Expect.within (Absolute 0.0001) 1.5
                    ]
        , test "clamp limits output value to the range" <|
            \() ->
                let
                    scale =
                        Scale.convert (Scale.radial ( 10, 20 ) ( 0, 1 ) |> Scale.clamp)
                in
                expectAll
                    [ scale 2
                        |> Expect.within (Absolute 0.0001) 20
                    , scale -1
                        |> Expect.within (Absolute 0.0001) 10
                    ]
        , fuzz (tuple3 ( tuple ( float, float ), tuple ( float, float ), float )) "rangeExtent returns the range" <|
            \( domain, range, val ) ->
                Scale.rangeExtent (Scale.radial range domain) |> Expect.equal range
        ]

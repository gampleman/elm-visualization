module Scale.PowerTests exposing (all)

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
    describe "Scale.power"
        [ test "convert maps a domain value x to range value y" <|
            \() ->
                Scale.convert (Scale.power 0.5 ( 0, 1 ) ( 0, 1 )) 0.5
                    |> Expect.within (Absolute 0.0001) (sqrt 0.5)
        , test "maps an empty domain to the middle of the range" <|
            \() ->
                expectAll
                    [ Scale.convert (Scale.power 1 ( 1, 2 ) ( 0, 0 )) 0
                        |> Expect.within (Absolute 0.0001) 1.5
                    , Scale.convert (Scale.power 1 ( 2, 1 ) ( 0, 0 )) 1
                        |> Expect.within (Absolute 0.0001) 1.5
                    ]
        , fuzz2 (Fuzz.floatRange 0.0001 20) (tuple3 ( tuple ( float, float ), tuple ( float, float ), nonZero )) "invert is the inverse of convert" <|
            \exponent ( ( r0, r1 ), ( d0, d1 ), val ) ->
                let
                    scale =
                        Scale.power exponent ( r0, r1 ) ( d0, d1 )

                    double =
                        Scale.convert scale val |> Scale.invert scale
                in
                if r0 == r1 || d0 == d1 then
                    -- this is a special case, since it needs to go into the middle
                    Expect.pass

                else
                    double
                        |> Expect.within (AbsoluteOrRelative 0.0001 0.01) val
        , fuzz2 nonZero (tuple3 ( tuple ( float, float ), tuple ( float, float ), float )) "clamp limits output value to the range" <|
            \exponent ( domain, range, val ) ->
                let
                    convert =
                        Scale.convert (Scale.clamp (Scale.power exponent range domain)) val
                in
                convert |> isBetween range
        , fuzz (tuple3 ( tuple ( float, float ), tuple ( float, float ), float )) "rangeExtent returns the range" <|
            \( domain, range, val ) ->
                Scale.rangeExtent (Scale.power 2 range domain) |> Expect.equal range

        -- Clamping is not performed for inversion yet due to type constraints
        -- , fuzz (tuple3 ( tuple ( float, float ), tuple ( float, float ), float )) "clamp limits output value to the range" <|
        --     \( domain, range, val ) ->
        --         let
        --             invert =
        --                 Scale.invert (Scale.clamp (Scale.linear domain range)) val
        --         in
        --             invert |> isBetween domain
        -- , describe "nice"
        --  [ test "small domain" <|
        --     () ->
        --       Scale.nice (Scale.linear (0, 0.96) (0, 1)) 10
        --   ]
        ]

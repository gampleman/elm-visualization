module Scale.ContinousTests exposing (linear, power, radial, symlog)

import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (..)
import Helper exposing (expectAll, isAbout, isBetween)
import Scale
import Test exposing (..)


linear : Test
linear =
    describe "Scale.linear" <|
        buildTests
            { convert = Scale.linear ( 1, 2 ) ( 0, 1 )
            , scale = Scale.linear
            , convertExpected = 1.5
            , convertEmptyExpected = 1.5
            , invertExpected = 0.5
            , invertEmptyExpected = 1.5
            }
            ++ [ fuzz (tuple3 ( tuple ( float, float ), tuple ( float, float ), float )) "invert is the inverse of convert" <|
                    \( ( r0, r1 ), ( d0, d1 ), val ) ->
                        let
                            scale =
                                Scale.linear ( r0, r1 ) ( d0, d1 )

                            double =
                                Scale.convert scale val |> Scale.invert scale
                        in
                        if r0 == r1 || d0 == d1 then
                            -- this is a special case, since it needs to go into the middle
                            double |> Expect.within (Absolute 0.0001) ((d0 + d1) / 2)

                        else
                            double
                                |> Expect.within (Absolute 0.01) val
               , fuzz (tuple3 ( tuple ( float, float ), tuple ( float, float ), float )) "clamp limits output value to the range (fuzz)" <|
                    \( domain, range, val ) ->
                        let
                            convert =
                                Scale.convert (Scale.clamp (Scale.linear range domain)) val
                        in
                        convert |> isBetween range
               ]


radial : Test
radial =
    describe "Scale.radial" <|
        buildTests
            { convert = Scale.radial ( 1, 2 ) ( 0, 1 )
            , scale = Scale.radial
            , convertExpected = 1.5811388300841898
            , convertEmptyExpected = 1.5811388300841898
            , invertExpected = 0.4166666666666667
            , invertEmptyExpected = 1.5
            }


power : Test
power =
    describe "Scale.power" <|
        buildTests
            { convert = Scale.power 0.5 ( 0, 1 ) ( 0, 1 )
            , scale = Scale.power 1
            , convertExpected = sqrt 0.5
            , convertEmptyExpected = 1.5
            , invertExpected = 0.5
            , invertEmptyExpected = 1.5
            }


symlog : Test
symlog =
    describe "Scale.symlog" <|
        buildTests
            { convert = Scale.symlog 1 ( 0, 1 ) ( 0, 0.5 )
            , scale = Scale.symlog 1
            , convertExpected = 1
            , convertEmptyExpected = 1.5
            , invertExpected = 0.41421356237309503
            , invertEmptyExpected = 1.449489742783178
            }
            ++ [ test "handles negatives" <|
                    \() ->
                        Scale.convert (Scale.symlog 1 ( 0, 1 ) ( -100, 100 )) -100
                            |> Expect.within (Absolute 0.0001) 0
               , test "handles zero" <|
                    \() ->
                        Scale.convert (Scale.symlog 1 ( 0, 1 ) ( -100, 100 )) 0
                            |> Expect.within (Absolute 0.0001) 0.5
               ]


buildTests { convert, scale, convertExpected, convertEmptyExpected, invertExpected, invertEmptyExpected } =
    [ test "convert maps a domain value x to range value y" <|
        \() ->
            Scale.convert convert 0.5
                |> Expect.within (Absolute 0.0001) convertExpected
    , test "maps an empty domain to the middle of the range" <|
        \() ->
            expectAll
                [ Scale.convert (scale ( 1, 2 ) ( 0, 0 )) 0
                    |> Expect.within (Absolute 0.0001) convertEmptyExpected
                , Scale.convert (scale ( 2, 1 ) ( 0, 0 )) 1
                    |> Expect.within (Absolute 0.0001) convertEmptyExpected
                ]
    , test "invert maps a range value y to a domain value x" <|
        \() ->
            Scale.invert (scale ( 1, 2 ) ( 0, 1 )) 1.5
                |> Expect.within (Absolute 0.00001) invertExpected
    , test "invert y maps an empty range to the middle of the domain" <|
        \() ->
            expectAll
                [ Scale.invert (scale ( 0, 0 ) ( 1, 2 )) 0
                    |> Expect.within (Absolute 0.0001) invertEmptyExpected
                , Scale.invert (scale ( 0, 0 ) ( 2, 1 )) 1
                    |> Expect.within (Absolute 0.0001) invertEmptyExpected
                ]
    , test "clamp limits output value to the range" <|
        \() ->
            let
                scaleFn =
                    Scale.convert (scale ( 10, 20 ) ( 0, 1 ) |> Scale.clamp)
            in
            expectAll
                [ scaleFn 2
                    |> Expect.within (Absolute 0.0001) 20
                , scaleFn -1
                    |> Expect.within (Absolute 0.0001) 10
                ]
    , fuzz (tuple3 ( tuple ( float, float ), tuple ( float, float ), float )) "rangeExtent returns the range" <|
        \( domain, range, val ) ->
            Scale.rangeExtent (scale range domain) |> Expect.equal range
    ]

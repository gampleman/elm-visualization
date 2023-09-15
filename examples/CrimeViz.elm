module CrimeViz exposing (main)

{-| This example shows a more opinionated style of a line chart.

  - The y axis tick marks show the starting values of each series.
  - We position and color a label next to each series.

@category Advanced

-}

import Axis
import Color exposing (Color)
import Path exposing (Path)
import Scale exposing (ContinuousScale, OrdinalScale)
import Scale.Color
import Shape
import Statistics
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (class, dy, fill, fontFamily, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (fontSize, strokeWidth, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..), Transform(..), em)


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    60


series : List { label : String, accessor : CrimeRate -> Int }
series =
    [ { label = "Murder"
      , accessor = .murder
      }
    , { label = "Rape"
      , accessor = .rape
      }
    , { label = "Robbery"
      , accessor = .robbery
      }
    , { label = "Assault"
      , accessor = .assault
      }
    ]


accessors : List (CrimeRate -> Int)
accessors =
    List.map .accessor series


values : CrimeRate -> List Float
values i =
    List.map (\a -> toFloat <| a i) accessors


colorScale : OrdinalScale String Color
colorScale =
    List.map .label series
        |> Scale.ordinal Scale.Color.category10


color : String -> Color
color =
    Scale.convert colorScale >> Maybe.withDefault Color.black


view : List CrimeRate -> Svg msg
view model =
    let
        last =
            List.reverse model
                |> List.head
                |> Maybe.withDefault (CrimeRate 0 0 0 0 0 0 0 0 0)

        first =
            List.head model
                |> Maybe.withDefault (CrimeRate 0 0 0 0 0 0 0 0 0)

        xScale : ContinuousScale Float
        xScale =
            model
                |> List.map (.year >> toFloat)
                |> Statistics.extent
                |> Maybe.withDefault ( 1900, 1901 )
                |> Scale.linear ( 0, w - 2 * padding )

        yScale : ContinuousScale Float
        yScale =
            model
                |> List.map (values >> List.maximum >> Maybe.withDefault 0)
                |> List.maximum
                |> Maybe.withDefault 0
                |> (\b -> ( 0, b ))
                |> Scale.linear ( h - 2 * padding, 0 )
                |> Scale.nice 4

        lineGenerator : ( Int, Int ) -> Maybe ( Float, Float )
        lineGenerator ( x, y ) =
            Just ( Scale.convert xScale (toFloat x), Scale.convert yScale (toFloat y) )

        line : (CrimeRate -> Int) -> Path
        line accessor =
            List.map (\i -> ( .year i, accessor i )) model
                |> List.map lineGenerator
                |> Shape.line Shape.monotoneInXCurve
    in
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ Axis.bottom [ Axis.tickCount 10 ] xScale ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ Axis.left [ Axis.ticks (values first) ] yScale
            , text_ [ fontFamily [ "sans-serif" ], fontSize 10, x 5, y 5 ] [ text "Occurences" ]
            ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ]
            (List.map
                (\{ accessor, label } ->
                    Path.element (line accessor)
                        [ stroke <| Paint <| color label
                        , strokeWidth 3
                        , fill PaintNone
                        ]
                )
                series
            )
        , g [ fontFamily [ "sans-serif" ], fontSize 10 ]
            (List.map
                (\{ accessor, label } ->
                    g
                        [ transform
                            [ Translate (w - padding + 10) (padding + Scale.convert yScale (toFloat (accessor last)))
                            ]
                        ]
                        [ text_ [ fill (Paint (color label)) ] [ text label ] ]
                )
                series
            )
        , g [ transform [ Translate (w - padding) (padding + 20) ] ]
            [ text_ [ fontFamily [ "sans-serif" ], fontSize 20, textAnchor AnchorEnd ] [ text "Violent Crime in the US" ]
            , text_ [ fontFamily [ "sans-serif" ], fontSize 10, textAnchor AnchorEnd, dy (em 1) ] [ text "Source: fbi.gov" ]
            ]
        ]


main : Svg msg
main =
    view crimeRates


type alias CrimeRate =
    { year : Int
    , population : Int
    , murder : Int
    , rape : Int
    , robbery : Int
    , assault : Int
    , burglary : Int
    , larceny : Int
    , motorTheft : Int
    }


crimeRates : List CrimeRate
crimeRates =
    [ CrimeRate 1994 260327021 23326 102216 618949 1113179 2712774 7879812 1539287
    , CrimeRate 1995 262803276 21606 97470 580509 1099207 2593784 7997710 1472441
    , CrimeRate 1996 265228572 19645 96252 535594 1037049 2506400 7904685 1394238
    , CrimeRate 1997 267783607 18208 96153 498534 1023201 2460526 7743760 1354189
    , CrimeRate 1998 270248003 16974 93144 447186 976583 2332735 7376311 1242781
    , CrimeRate 1999 272690813 15522 89411 409371 911740 2100739 6955520 1152075
    , CrimeRate 2000 281421906 15586 90178 408016 911706 2050992 6971590 1160002
    , CrimeRate 2001 285317559 16037 90863 423557 909023 2116531 7092267 1228391
    , CrimeRate 2002 287973924 16229 95235 420806 891407 2151252 7057379 1246646
    , CrimeRate 2003 290788976 16528 93883 414235 859030 2154834 7026802 1261226
    , CrimeRate 2004 293656842 16148 95089 401470 847381 2144446 6937089 1237851
    , CrimeRate 2005 296507061 16740 94347 417438 862220 2155448 6783447 1235859
    , CrimeRate 2006 299398484 17309 94472 449246 874096 2194993 6626363 1198245
    , CrimeRate 2007 301621157 17128 92160 447324 866358 2190198 6591542 1100472
    , CrimeRate 2008 304059724 16465 90750 443563 843683 2228887 6586206 959059
    , CrimeRate 2009 307006550 15399 89241 408742 812514 2203313 6338095 795652
    , CrimeRate 2010 309330219 14722 85593 369089 781844 2168459 6204601 739565
    , CrimeRate 2011 311587816 14661 84175 354746 752423 2185140 6151095 716508
    , CrimeRate 2012 313873685 14856 85141 355051 762009 2109932 6168874 723186
    , CrimeRate 2013 316128839 14196 79770 345031 724149 1928465 6004453 699594
    ]

module PopulationMinnesota exposing (main)

{-| Shows a vertically stacked bar chart.

@category Advanced

-}

import Axis
import Color exposing (Color)
import List.Extra as List
import Scale exposing (BandScale, ContinuousScale, Scale, defaultBandConfig)
import Scale.Color
import Shape exposing (StackConfig, StackResult)
import Statistics
import TypedSvg exposing (g, rect, svg, text_)
import TypedSvg.Attributes exposing (class, dy, fill, fontFamily, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (fontSize, height, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..), Transform(..), em)


main : Svg msg
main =
    view (Shape.stack config)


transformedData : { categories : List Int, data : List ( Int, List Float ), extent : ( Float, Float ) }
transformedData =
    let
        partitioned =
            -- assumes sorted by year, then age
            populationMinnesota1850
                |> List.filter (\{ year } -> year == 1850)
                |> List.partition (\{ gender } -> gender == M)

        categories =
            partitioned
                |> Tuple.first
                |> List.map .age
                |> List.reverse

        ( m, f ) =
            partitioned
                |> (\( a, b ) -> List.map2 Tuple.pair a b)
                |> List.sortBy (Tuple.second >> .people)
                |> List.unzip
    in
    { categories = categories
    , data = [ ( 1850, List.map (.people >> toFloat) m ), ( 1850, List.map (.people >> toFloat >> negate) f ) ]
    , extent =
        Statistics.extent categories
            |> Maybe.map (\( a, b ) -> ( toFloat a, toFloat b ))
            |> Maybe.withDefault ( 0, 0 )
    }


w : Float
w =
    990


h : Float
h =
    504


padding : number
padding =
    60


config : StackConfig Int
config =
    { data = transformedData.data
    , offset = Shape.stackOffsetDiverging
    , order = identity
    }


colors : List Color
colors =
    [ Scale.Color.viridisInterpolator 0.3
    , Scale.Color.viridisInterpolator 0.7
    ]


column : BandScale Int -> ( Int, List ( Float, Float ) ) -> Svg msg
column yScale ( year, values ) =
    let
        bandwidth =
            Scale.bandwidth yScale

        block color ( upperY, lowerY ) =
            rect
                [ y <| Scale.convert yScale year
                , x lowerY
                , height bandwidth
                , width (abs <| upperY - lowerY)
                , fill <| Paint color
                ]
                []
    in
    values
        |> List.map2 block colors
        |> g [ class [ "column" ] ]


view : StackResult Int -> Svg msg
view { values, extent } =
    let
        scaledValues =
            values
                |> List.transpose
                |> List.map (List.map (\( y1, y2 ) -> ( Scale.convert xScale y1, Scale.convert xScale y2 )))

        xScale : ContinuousScale Float
        xScale =
            Scale.linear ( w - padding, padding ) extent
                |> Scale.nice 4

        yScale : BandScale Int
        yScale =
            Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } ( padding, h - padding ) transformedData.categories
    in
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate 0 (h - padding) ] ]
            [ Axis.bottom [ Axis.tickFormat (absoluteTickFormat xScale 10) ] xScale ]
        , g [ class [ "series" ] ] <|
            List.map (column yScale) (List.map2 (\a b -> ( a, b )) transformedData.categories scaledValues)
        , g [ transform [ Translate padding 0 ] ]
            [ Axis.left [] (Scale.toRenderable String.fromInt yScale)
            , text_ [ fontFamily [ "sans-serif" ], fontSize 14, x 5, y 65 ] [ text "Age" ]
            ]
        , g [ transform [ Translate (w - padding) (padding + 20) ] ]
            [ text_ [ fontFamily [ "sans-serif" ], fontSize 20, textAnchor AnchorEnd ] [ text "1850" ]
            , text_ [ fontFamily [ "sans-serif" ], fontSize 10, textAnchor AnchorEnd, dy (em 1) ] [ text "population distribution in Minnesota" ]
            ]
        , text_
            [ transform [ Translate (Scale.convert xScale 500000) (2 * padding) ]
            , fontFamily [ "sans-serif" ]
            , fontSize 20
            , textAnchor AnchorMiddle
            ]
            [ text "Men" ]
        , text_
            [ transform [ Translate (Scale.convert xScale -500000) (2 * padding) ]
            , fontFamily [ "sans-serif" ]
            , fontSize 20
            , textAnchor AnchorMiddle
            ]
            [ text "Women" ]
        , text_
            [ transform [ Translate (Scale.convert xScale 0) (h - padding / 2) ]
            , fontFamily [ "sans-serif" ]
            , fontSize 14
            , textAnchor AnchorMiddle
            , dy (em 1)
            ]
            [ text "People" ]
        ]


{-| Make negative numbers appear as positive in the ticks
-}
absoluteTickFormat :
    Scale
        { a
            | convert : domain -> range -> number -> b
            , domain : domain
            , tickFormat : domain -> Int -> number -> String
        }
    -> Int
    -> number
    -> String
absoluteTickFormat scale tickCount value =
    Scale.tickFormat scale tickCount (abs value)


type Gender
    = M
    | F


type alias Population =
    { year : Int, age : Int, gender : Gender, people : Int }


populationMinnesota1850 : List Population
populationMinnesota1850 =
    [ Population 1850 0 M 1483789
    , Population 1850 0 F 1450376
    , Population 1850 5 M 1411067
    , Population 1850 5 F 1359668
    , Population 1850 10 M 1260099
    , Population 1850 10 F 1216114
    , Population 1850 15 M 1077133
    , Population 1850 15 F 1110619
    , Population 1850 20 M 1017281
    , Population 1850 20 F 1003841
    , Population 1850 25 M 862547
    , Population 1850 25 F 799482
    , Population 1850 30 M 730638
    , Population 1850 30 F 639636
    , Population 1850 35 M 588487
    , Population 1850 35 F 505012
    , Population 1850 40 M 475911
    , Population 1850 40 F 428185
    , Population 1850 45 M 384211
    , Population 1850 45 F 341254
    , Population 1850 50 M 321343
    , Population 1850 50 F 286580
    , Population 1850 55 M 194080
    , Population 1850 55 F 187208
    , Population 1850 60 M 174976
    , Population 1850 60 F 162236
    , Population 1850 65 M 106827
    , Population 1850 65 F 105534
    , Population 1850 70 M 73677
    , Population 1850 70 F 71762
    , Population 1850 75 M 40834
    , Population 1850 75 F 40229
    , Population 1850 80 M 23449
    , Population 1850 80 F 22949
    , Population 1850 85 M 8186
    , Population 1850 85 F 10511
    , Population 1850 90 M 5259
    , Population 1850 90 F 6569
    ]

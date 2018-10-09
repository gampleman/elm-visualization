module PopulationMinnesota exposing (main)

import Axis
import Color exposing (Color)
import List.Extra as List
import SampleData exposing (Gender(..))
import Scale exposing (BandScale, ContinuousScale, OrdinalScale, QuantizeScale, Scale, defaultBandConfig)
import Scale.Color
import Shape exposing (StackConfig, StackResult)
import Statistics
import TypedSvg exposing (g, rect, svg, text_)
import TypedSvg.Attributes exposing (class, dy, fill, fontFamily, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (fontSize, height, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Transform(..), em)


main : Svg msg
main =
    view (Shape.stack config)


populationMinnesota1850 : { categories : List Int, data : List ( Int, List Float ), extent : ( Float, Float ) }
populationMinnesota1850 =
    let
        partitioned =
            -- assumes sorted by year, then age
            SampleData.populationMinnesota1850
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
    { data = populationMinnesota1850.data
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
                , fill (Fill color)
                ]
                []
    in
    values
        |> List.map2 block colors
        |> g [ class [ "column" ] ]


view : StackResult Int -> Svg msg
view { values, labels, extent } =
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
            Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } ( padding, h - padding ) populationMinnesota1850.categories
    in
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate 0 (h - padding) ] ]
            [ Axis.bottom [ Axis.tickFormat (absoluteTickFormat xScale 10) ] xScale ]
        , g [ class [ "series" ] ] <|
            List.map (column yScale) (List.map2 (\a b -> ( a, b )) populationMinnesota1850.categories scaledValues)
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

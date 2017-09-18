module PopulationMinnesota exposing (main)

import SampleData exposing (Gender(M, F))
import Color.Convert exposing (colorToCssRgba)
import Visualization.Scale as Scale exposing (ContinuousScale, OrdinalScale, Scale, BandScale, defaultBandConfig, QuantizeScale)
import Visualization.Axis as Axis exposing (Orientation(..))
import List.Extra as List
import Visualization.List
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Visualization.Shape as Shape exposing (StackConfig, StackResult)


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
                |> uncurry (List.map2 (,))
                |> List.sortBy (Tuple.second >> .people)
                |> List.unzip
    in
        { categories = categories
        , data = [ ( 1850, List.map (.people >> toFloat) m ), ( 1850, List.map (.people >> toFloat >> negate) f ) ]
        , extent =
            Visualization.List.extent categories
                |> Maybe.map (\( a, b ) -> ( toFloat a, toFloat b ))
                |> Maybe.withDefault ( 0, 0 )
        }


width : number
width =
    990


height : number
height =
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


colors : List String
colors =
    [ Scale.viridisInterpolator 0.3
    , Scale.viridisInterpolator 0.7
    ]
        |> List.map colorToCssRgba


column : BandScale Int -> ( Int, List ( Float, Float ) ) -> Svg msg
column yScale ( year, values ) =
    let
        bandwidth =
            Scale.bandwidth yScale

        block color ( upperY, lowerY ) =
            rect
                [ y <| toString <| Scale.convert yScale year
                , x <| toString <| lowerY
                , Svg.Attributes.height <| toString <| bandwidth
                , Svg.Attributes.width <| toString <| (abs <| upperY - lowerY)
                , fill color
                ]
                []
    in
        values
            |> List.map2 block colors
            |> g [ class "column" ]


view : StackResult Int -> Svg msg
view { values, labels, extent } =
    let
        scaledValues =
            values
                |> List.transpose
                |> List.map (List.map (\( y1, y2 ) -> ( Scale.convert xScale y1, Scale.convert xScale y2 )))

        axisOptions =
            Axis.defaultOptions

        xScale : ContinuousScale
        xScale =
            Scale.linear extent ( width - padding, padding )
                |> flip Scale.nice 4

        yScale : BandScale Int
        yScale =
            Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } populationMinnesota1850.categories ( padding, height - padding )

        xAxis : Svg msg
        xAxis =
            Axis.axis { axisOptions | orientation = Axis.Bottom, tickFormat = Just (absoluteTickFormat xScale 10) } xScale

        yAxis : Svg msg
        yAxis =
            let
                tickCount =
                    populationMinnesota1850
                        |> .extent
                        |> Tuple.second
                        |> (\v -> round v // 10 * 2)
            in
                Axis.axis { axisOptions | orientation = Axis.Left } (Scale.toRenderable yScale)
    in
        Svg.svg [ Svg.Attributes.width (toString width ++ "px"), Svg.Attributes.height (toString height ++ "px") ]
            [ g [ translate 0 (height - padding) ]
                [ xAxis ]
            , g [ class "series" ] <|
                List.map (column yScale) (List.map2 (,) populationMinnesota1850.categories scaledValues)
            , g [ translate padding 0 ]
                [ yAxis
                , text_ [ fontFamily "sans-serif", fontSize "14", x "5", y "65" ] [ text "Age" ]
                ]
            , g [ translate (width - padding) (padding + 20) ]
                [ text_ [ fontFamily "sans-serif", fontSize "20", textAnchor "end" ] [ text "1850" ]
                , text_ [ fontFamily "sans-serif", fontSize "10", textAnchor "end", dy "1em" ] [ text "population distribution in Minnesota" ]
                ]
            , text_
                [ translate (Scale.convert xScale 500000) (2 * padding)
                , fontFamily "sans-serif"
                , fontSize "20"
                , textAnchor "middle"
                ]
                [ text "Men" ]
            , text_
                [ translate (Scale.convert xScale -500000) (2 * padding)
                , fontFamily "sans-serif"
                , fontSize "20"
                , textAnchor "middle"
                ]
                [ text "Women" ]
            , text_
                [ translate (Scale.convert xScale 0) (height - padding / 2)
                , fontFamily "sans-serif"
                , fontSize "14"
                , textAnchor "middle"
                , dy "1em"
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


translate : number -> number -> Svg.Attribute msg
translate x y =
    transform ("translate(" ++ toString x ++ ", " ++ toString y ++ ")")

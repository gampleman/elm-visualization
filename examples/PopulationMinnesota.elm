module DutchTemperatures exposing (..)

import SampleData exposing (Gender(M, F))
import Color.Convert exposing (colorToCssRgba)
import Visualization.Scale as Scale exposing (ContinuousScale, OrdinalScale, Scale, BandScale, defaultBandConfig, QuantizeScale)
import Visualization.Axis as Axis exposing (Orientation(..))
import List.Extra as List
import Visualization.List
import Svg exposing (..)
import Html exposing (div)
import Svg.Attributes exposing (..)
import Visualization.Shape as Shape exposing (StackConfig, StackResult)


main : Svg msg
main =
    div []
        [ view (Shape.stack config)
        ]


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


canvas : { width : Float, height : Float }
canvas =
    { width = 900
    , height = 450
    }


padding : { bottom : number, left : number1, right : number2, top : number3 }
padding =
    { top = 60
    , left = 60
    , right = 60
    , bottom = 60
    }


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


column : ( Int, List ( Float, Float ) ) -> Svg msg
column ( year, values ) =
    let
        barHeight =
            15

        scale =
            Scale.linear populationMinnesota1850.extent ( canvas.height - (padding.top + padding.bottom + barHeight), 0 )

        block color ( upperY, lowerY ) =
            rect
                [ y <| toString <| Scale.convert scale (toFloat year)
                , x <| toString <| lowerY
                , height <| toString <| barHeight
                , width <| toString <| (abs <| upperY - lowerY)
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

        xScale =
            Scale.linear extent ( canvas.width - (padding.top + padding.bottom), 0 )
                |> flip Scale.nice 4

        yScale =
            Scale.linear populationMinnesota1850.extent ( canvas.height - (padding.left + padding.right), 0 )

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
                Axis.axis { axisOptions | orientation = Axis.Left, tickCount = tickCount } yScale
    in
        Svg.svg [ width (toString canvas.width ++ "px"), height (toString canvas.height ++ "px") ]
            [ g [ translate (padding.left - 1) (canvas.height - padding.top) ]
                [ xAxis ]
            , g [ translate padding.left padding.top, class "series" ] <|
                List.map column (List.map2 (,) populationMinnesota1850.categories scaledValues)
            , g [ translate (padding.left - 1) padding.top ]
                [ yAxis
                , text_ [ fontFamily "sans-serif", fontSize "18", x "5", y "5" ] [ text "Age" ]
                ]
            , g [ translate (canvas.width - padding.right) (padding.top + 20) ]
                [ text_ [ fontFamily "sans-serif", fontSize "20", textAnchor "end" ] [ text "1850" ]
                , text_ [ fontFamily "sans-serif", fontSize "10", textAnchor "end", dy "1em" ] [ text "population distribution in Minnesota" ]
                ]
            , text_
                [ translate (padding.left + Scale.convert xScale 500000) (2 * padding.top)
                , fontFamily "sans-serif"
                , fontSize "20"
                , textAnchor "middle"
                ]
                [ text "Males" ]
            , text_
                [ translate (padding.left + Scale.convert xScale -500000) (2 * padding.top)
                , fontFamily "sans-serif"
                , fontSize "20"
                , textAnchor "middle"
                ]
                [ text "Females" ]
            , text_
                [ translate (padding.left + Scale.convert xScale 0) (canvas.height - padding.bottom / 2)
                , fontFamily "sans-serif"
                , fontSize "18"
                , textAnchor "middle"
                , dy "1em"
                ]
                [ text "People" ]
            ]


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

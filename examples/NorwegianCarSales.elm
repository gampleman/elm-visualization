module NorwegianCarSales exposing (..)

import SampleData
import Color.Convert exposing (colorToCssRgb)
import Visualization.Scale as Scale exposing (ContinuousScale, OrdinalScale, Scale)
import Visualization.Axis as Axis exposing (Orientation(..))
import List.Extra as List
import Color exposing (Color)
import Svg exposing (..)
import Html exposing (div)
import Svg.Attributes exposing (..)
import Visualization.Shape as Shape exposing (StackConfig, StackResult)
import Date exposing (Date, Month(..))
import Date.Extra as Date


main : Svg msg
main =
    div []
        [ view (Shape.stack streamgraphConfig)
        , view (Shape.stack silhouetteConfig)
        , view (Shape.stack stackedGraphConfig)
        ]


samples : List ( String, List Float )
samples =
    SampleData.norwegianCarSalesMiddlePlayers


colorScale : OrdinalScale String Color
colorScale =
    let
        colors =
            Scale.linear ( 0.1, 1.0 ) ( 0, 1 )
                |> flip Scale.ticks (List.length samples + 1)
                |> List.map (\v -> Scale.viridisInterpolator (1 - v))
    in
        Scale.ordinal (List.reverse <| List.map Tuple.first samples) colors


sampleColor : String -> Color
sampleColor label =
    Scale.convert colorScale label |> Maybe.withDefault Color.black


colors : List String -> List Color
colors labels =
    List.map sampleColor labels


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


expandConfig : StackConfig String
expandConfig =
    -- this config is not displayed correctly by the code in this file
    -- because we process a subset of the data, but still display the full x-axis
    { data =
        List.take 5 samples
            |> List.map (Tuple.mapSecond <| List.take <| 12 * 3)
    , offset =
        Shape.stackOffsetExpand
    , order = List.sortBy (Tuple.second >> List.sum >> negate)
    }


stackedGraphConfig : StackConfig String
stackedGraphConfig =
    { data = samples
    , offset = Shape.stackOffsetNone
    , order = List.sortBy (Tuple.second >> List.sum >> negate)
    }


streamgraphConfig : StackConfig String
streamgraphConfig =
    { data = samples
    , offset = Shape.stackOffsetWiggle
    , order = Shape.sortByInsideOut (Tuple.second >> List.sum)
    }


silhouetteConfig : StackConfig String
silhouetteConfig =
    { data = samples
    , offset = Shape.stackOffsetSilhouette
    , order = Shape.sortByInsideOut (Tuple.second >> List.sum)
    }


config : StackConfig String
config =
    streamgraphConfig


view : StackResult String -> Svg msg
view { values, labels, extent } =
    let
        size : Int
        size =
            List.head values
                |> Maybe.map List.length
                |> Maybe.withDefault 0

        xScale : ContinuousScale
        xScale =
            -- map an index to screen space
            Scale.linear ( 0, toFloat size - 1 ) ( 0, canvas.width - (padding.top + padding.bottom) )

        yScale : ContinuousScale
        yScale =
            Scale.linear extent ( canvas.height - (padding.left + padding.right), 0 )
                |> flip Scale.nice 4

        axisOptions =
            Axis.defaultOptions

        xAxis : Svg msg
        xAxis =
            -- construct the time domain for display
            -- the data is per-month, so we have to pick a day
            -- to get the ticks to show up correctly, the upper bound needs to be Jan 2 (Jan 1 does not work).
            Scale.time ( Date.fromCalendarDate 2007 Jan 1, Date.fromCalendarDate 2017 Jan 2 ) ( 0, canvas.width - (padding.top + padding.bottom) )
                |> Axis.axis { axisOptions | orientation = Axis.Bottom, tickCount = 1 }

        yAxis : Svg msg
        yAxis =
            Axis.axis { axisOptions | orientation = Axis.Left } yScale

        paths =
            List.map2 (renderStream ( xScale, yScale )) (colors labels) values

        labelPositions =
            let
                position ys =
                    ys
                        |> List.last
                        |> Maybe.withDefault ( 0, 0 )
                        |> (\( y1, y2 ) -> (y2 + y1) / 2)
                        |> Scale.convert yScale
            in
                List.map position values

        labelElement : String -> Float -> Svg msg
        labelElement label yPosition =
            g [ translate (canvas.width - padding.right + 10) (padding.top + yPosition) ]
                [ text_ [ fill (sampleColor label |> colorToCssRgb) ] [ text label ] ]
    in
        Svg.svg [ width (toString canvas.width ++ "px"), height (toString canvas.height ++ "px") ]
            [ g [ translate (padding.left - 1) (canvas.height - padding.top) ]
                [ xAxis ]
            , g [ translate padding.left padding.top, class "series" ] paths
            , g [ translate (padding.left - 1) padding.top ]
                []
              -- [ yAxis, text_ [ fontFamily "sans-serif", fontSize "10", x "5", y "5" ] [ text "Occurences" ] ]
            , g [ fontFamily "sans-serif", fontSize "10" ]
                (List.map2 labelElement labels labelPositions)
            , g [ translate (canvas.width - padding.right) (padding.top + 20) ]
                [ text_ [ fontFamily "sans-serif", fontSize "20", textAnchor "end" ] [ text "Car Sales in Norway" ]
                  -- , text_ [ fontFamily "sans-serif", fontSize "10", textAnchor "end", dy "1em" ] [ text "Source: " ]
                ]
            ]


{-| Renders one colored stream with given scaling
-}
renderStream : ( ContinuousScale, ContinuousScale ) -> Color -> List ( Float, Float ) -> Svg msg
renderStream scales color coords =
    Svg.path [ fill (colorToCssRgb color), d (toArea scales coords) ] []


{-| Create a svg path string that draws the area between two lines
-}
toArea : ( ContinuousScale, ContinuousScale ) -> List ( Float, Float ) -> String
toArea ( scaleX, scaleY ) ys =
    let
        mapper : Int -> ( Float, Float ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
        mapper index ( y1, y2 ) =
            let
                xCoord =
                    index
                        |> toFloat
                        |> Scale.convert scaleX

                ( low, high ) =
                    if y1 < y2 then
                        ( y1, y2 )
                    else
                        ( y2, y1 )
            in
                Just
                    ( ( xCoord, Scale.convert scaleY low )
                    , ( xCoord, Scale.convert scaleY high )
                    )
    in
        List.indexedMap mapper ys
            |> Shape.area Shape.monotoneInXCurve


translate : number -> number -> Svg.Attribute msg
translate x y =
    transform ("translate(" ++ toString x ++ ", " ++ toString y ++ ")")

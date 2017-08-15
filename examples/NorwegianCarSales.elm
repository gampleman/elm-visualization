module NorwegianCarSales exposing (..)

import SampleData
import Color.Convert exposing (colorToCssRgb)
import Visualization.Scale as Scale exposing (ContinuousScale, OrdinalScale, Scale)
import Visualization.Axis as Axis exposing (Orientation(..))
import List.Extra as List
import Visualization.List
import Color exposing (Color)
import Svg exposing (..)
import Html exposing (div)
import Svg.Attributes exposing (..)
import Visualization.Shape as Shape exposing (StackConfig, StackResult)
import Date exposing (Date, Month(..))
import Date.Extra as Date
import Dict


main : Svg msg
main =
    div []
        [ view (Shape.stack streamgraphConfig)
        , view (Shape.stack silhouetteConfig)
        , view (Shape.stack stackedGraphConfig)
        ]


type alias Year =
    Int


samples : List ( String, List Float )
samples =
    SampleData.norwegianCarSales
        |> Dict.map
            (\make years ->
                years
                    |> Dict.map
                        (\year months ->
                            List.range 1 12
                                |> List.map (flip Dict.get months >> Maybe.withDefault 0 >> toFloat)
                        )
            )
        |> Dict.map
            (\make years ->
                List.range 2007 2016
                    |> List.map (flip Dict.get years >> Maybe.withDefault (List.repeat 12 0))
                    |> List.concat
            )
        |> Dict.toList
        |> List.sortBy (Tuple.second >> List.sum >> negate)
        |> List.drop 4
        |> List.take 8


colorScale : OrdinalScale String Color
colorScale =
    let
        colors =
            Scale.linear ( 0.1, 1.0 ) ( 0, 1 )
                |> flip Scale.ticks (List.length samples + 1)
                |> List.map (\v -> Scale.viridisInterpolator (1 - v))
    in
        Scale.ordinal (List.reverse <| List.map Tuple.first samples) colors



-- Scale.category20b


sampleColor : String -> Color
sampleColor label =
    Scale.convert colorScale label
        |> Maybe.withDefault Color.black


colors : List String -> List Color
colors labels =
    let
        size =
            List.length labels

        -- given an index, give a value in [0,1] (0 for first, 1 for final)
        lengthScale =
            Scale.linear ( 0, toFloat size - 1 ) ( 0, 1 )
                |> Scale.convert
    in
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
        years : List Year
        years =
            List.map .year SampleData.crimeRates

        --xScale : ContinuousScale
        xScale =
            let
                domain =
                    Visualization.List.extent years
                        |> Maybe.withDefault ( 0, 0 )
                        |> (\( a, b ) -> ( toFloat a, toFloat b ))
            in
                Scale.linear ( 0, 10 * 52 ) ( 0, canvas.width - (padding.top + padding.bottom) )

        -- Scale.time ( movies.start, movies.end ) ( 0, canvas.width - (padding.top + padding.bottom) )
        yScale : ContinuousScale
        yScale =
            Scale.linear extent ( canvas.height - (padding.left + padding.right), 0 )
                |> flip Scale.nice 4

        axisOptions =
            Axis.defaultOptions

        timeAxis =
            Scale.time ( Date.fromCalendarDate 2007 Jan 1, Date.fromCalendarDate 2017 Jan 2 ) ( 0, canvas.width - (padding.top + padding.bottom) )
                |> Axis.axis { axisOptions | orientation = Axis.Bottom, tickCount = 1 }

        xAxis : Svg msg
        xAxis =
            Axis.axis { axisOptions | orientation = Axis.Bottom } xScale

        yAxis : Svg msg
        yAxis =
            Axis.axis { axisOptions | orientation = Axis.Left {- , ticks = Just () -} } yScale

        paths =
            List.map2 (renderStream ( xScale, yScale )) (colors labels) values

        labelPositions =
            List.map (List.last >> Maybe.withDefault ( 0, 0 ) >> (\( y1, y2 ) -> (y2 + y1) / 2)) values
                |> List.map (Scale.convert yScale)
    in
        Svg.svg [ width (toString canvas.width ++ "px"), height (toString canvas.height ++ "px") ]
            [ g [ translate (padding.left - 1) (canvas.height - padding.top) ]
                [ timeAxis ]
            , g [ translate padding.left padding.top, class "series" ] paths
            , g [ translate (padding.left - 1) padding.top ]
                []
              -- [ yAxis, text_ [ fontFamily "sans-serif", fontSize "10", x "5", y "5" ] [ text "Occurences" ] ]
            , g [ fontFamily "sans-serif", fontSize "10" ]
                (List.map2
                    (\label yPosition ->
                        g [ translate (canvas.width - padding.right + 10) (padding.top + yPosition) ]
                            [ text_ [ fill (sampleColor label |> colorToCssRgb) ] [ text label ] ]
                    )
                    labels
                    labelPositions
                )
            , g [ translate (canvas.width - padding.right) (padding.top + 20) ]
                [ text_ [ fontFamily "sans-serif", fontSize "20", textAnchor "end" ] [ text "Car Sales in Norway" ]
                  -- , text_ [ fontFamily "sans-serif", fontSize "10", textAnchor "end", dy "1em" ] [ text "Source: " ]
                ]
            ]


{-| Renders one colored stream with given scaling

The result is a generator because we generate the  band color randomly
-}
renderStream : ( ContinuousScale, ContinuousScale ) -> Color -> List ( Float, Float ) -> Svg msg
renderStream scales color coords =
    -- Path.svgPath (Stack.toArea curve scales coords) [ fill (colorToCssRgb color) ]
    let
        pathString =
            toArea scales coords
    in
        Svg.path [ fill (colorToCssRgb color), d pathString ] []


{-| Create a svg path string that draws the area between two lines
-}
toArea : ( ContinuousScale, ContinuousScale ) -> List ( Float, Float ) -> String
toArea ( scaleX, scaleY ) ys =
    let
        mapper : Float -> ( Float, Float ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
        mapper xCoord ( y1, y2 ) =
            let
                ( l, h ) =
                    if y1 < y2 then
                        ( y1, y2 )
                    else
                        ( y2, y1 )
            in
                Just
                    ( ( xCoord, Scale.convert scaleY l )
                    , ( xCoord, Scale.convert scaleY h )
                    )

        xCoordinates : List Float
        xCoordinates =
            -- generate evenly spaced values in x's domain
            -- then cast them to values in the codomain/range
            evenlySpaced (List.length ys - 1) (Scale.domain scaleX)
                |> List.map (Scale.convert scaleX)
    in
        List.map2 mapper xCoordinates ys
            |> Shape.area Shape.monotoneInXCurve


{-| Give exact evenly spaced values in the give domain

    evenlySpaced 2 (0, 100) --> [ 0, 50, 100 ]

-}
evenlySpaced : Int -> ( Float, Float ) -> List Float
evenlySpaced n (( lower, upper ) as extent) =
    if n <= 1 then
        [ lower, upper ]
    else
        Scale.linear ( 1, toFloat n ) extent
            |> (\intermediateScale -> List.map (Scale.convert intermediateScale << toFloat) (List.range 1 n))


translate : number -> number -> Svg.Attribute msg
translate x y =
    transform ("translate(" ++ toString x ++ ", " ++ toString y ++ ")")

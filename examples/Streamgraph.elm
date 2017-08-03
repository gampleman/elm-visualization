module Streamgraph exposing (..)

import SampleData exposing (CrimeRate)
import Color.Convert exposing (colorToCssRgb)
import Visualization.Scale as Scale exposing (ContinuousScale, OrdinalScale, BandScale, defaultBandConfig, Scale)
import Visualization.Axis as Axis exposing (Orientation(..))
import List.Extra as List
import Visualization.List
import Color exposing (Color)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Visualization.Shape as Shape exposing (StackConfig, StackResult)


main : Svg msg
main =
    view (Shape.stack config)


type alias Year =
    Int


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
      {-
         , { label = "Larceny"
           , accessor = .larceny
           }
      -}
    , { label = "Motor Theft"
      , accessor = .motorTheft
      }
    ]


samples : List ( String, List Float )
samples =
    List.map (\{ label, accessor } -> ( label, List.map (toFloat << accessor) SampleData.crimeRates )) series


colorScale : OrdinalScale String Color
colorScale =
    Scale.ordinal (List.map .label series) Scale.category20c


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


config : StackConfig String
config =
    streamgraphConfig


view : StackResult String -> Svg msg
view { values, labels, extent } =
    let
        years : List Year
        years =
            List.map .year SampleData.crimeRates

        xScale : ContinuousScale
        xScale =
            let
                domain =
                    Visualization.List.extent years
                        |> Maybe.withDefault ( 0, 0 )
                        |> (\( a, b ) -> ( toFloat a, toFloat b ))
            in
                Scale.linear domain ( 0, canvas.width - (padding.top + padding.bottom) )

        yScale : ContinuousScale
        yScale =
            Scale.linear extent ( canvas.height - (padding.left + padding.right), 0 )
                |> flip Scale.nice 4

        axisOptions =
            Axis.defaultOptions

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
                [ xAxis ]
            , g [ translate padding.left padding.top, class "series" ] paths
            , g [ translate (padding.left - 1) padding.top ]
                [ yAxis, text_ [ fontFamily "sans-serif", fontSize "10", x "5", y "5" ] [ text "Occurences" ] ]
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
                [ text_ [ fontFamily "sans-serif", fontSize "20", textAnchor "end" ] [ text "Violent Crime in the US" ]
                , text_ [ fontFamily "sans-serif", fontSize "10", textAnchor "end", dy "1em" ] [ text "Source: fbi.gov" ]
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
            |> Shape.area Shape.linearCurve


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

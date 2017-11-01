module NorwegianCarSales exposing (main)

{-| This example demonstates using different kinds of layouts for stacked graphs.
-}

import SampleData
import Color.Convert exposing (colorToCssRgb)
import Visualization.Scale as Scale exposing (ContinuousScale, OrdinalScale, Scale)
import Visualization.Axis as Axis exposing (Orientation(..))
import List.Extra as List
import Color exposing (Color)
import Svg exposing (..)
import Html exposing (div)
import Html.Attributes
import Svg.Attributes exposing (..)
import Visualization.Shape as Shape exposing (StackConfig, StackResult)
import Date exposing (Date, Month(..))
import Date.Extra as Date
import Example


exampleConfig : List ( String, StackConfig String )
exampleConfig =
    [ ( "Stream Graph"
      , { data = samples
        , offset = Shape.stackOffsetWiggle
        , order = Shape.sortByInsideOut (Tuple.second >> List.sum)
        }
      )
    , ( "Sillhoutte"
      , { data = samples
        , offset = Shape.stackOffsetSilhouette
        , order = Shape.sortByInsideOut (Tuple.second >> List.sum)
        }
      )
    , ( "Stacked Area"
      , { data = samples
        , offset = Shape.stackOffsetNone
        , order = List.sortBy (Tuple.second >> List.sum >> negate)
        }
      )
    ]


samples : List ( String, List Float )
samples =
    SampleData.norwegianCarSalesMiddlePlayers


colorScale : OrdinalScale String Color
colorScale =
    Scale.ordinal (List.reverse <| List.map Tuple.first samples) Scale.category20c


sampleColor : String -> Color
sampleColor label =
    Scale.convert colorScale label |> Maybe.withDefault Color.black


colors : List String -> List Color
colors labels =
    List.map sampleColor labels


width : number
width =
    990


height : number
height =
    504


padding : number
padding =
    40


view : StackResult String -> Svg String
view { values, labels, extent } =
    let
        labelsWidth =
            50

        size : Int
        size =
            List.head values
                |> Maybe.map List.length
                |> Maybe.withDefault 0

        xScale : ContinuousScale
        xScale =
            -- map an index to screen space
            Scale.linear ( 0, toFloat size - 1 ) ( padding, width - padding - labelsWidth )

        yScale : ContinuousScale
        yScale =
            Scale.linear extent ( height - padding, padding )
                |> flip Scale.nice 4

        axisOptions =
            Axis.defaultOptions

        xAxis : Svg msg
        xAxis =
            -- construct the time domain for display
            -- the data is per-month, so we have to pick a day
            -- to get the ticks to show up correctly, the upper bound needs to be Jan 2 (Jan 1 does not work).
            Scale.time ( Date.fromCalendarDate 2007 Jan 1, Date.fromCalendarDate 2017 Jan 2 ) ( 0, width - padding * 2 - labelsWidth )
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
            g [ translate (width - padding - labelsWidth + 10) yPosition ]
                [ text_ [ fill (sampleColor label |> colorToCssRgb) ] [ text label ] ]
    in
        div []
            [ titleNavigation
            , Svg.svg [ Svg.Attributes.width (toString width ++ "px"), Svg.Attributes.height (toString height ++ "px") ]
                [ g [ translate (padding - 1) (height - padding) ]
                    [ xAxis ]
                , g [ class "series" ] paths
                , g [ fontFamily "sans-serif", fontSize "10" ]
                    (List.map2 labelElement labels labelPositions)
                ]
            ]


titleNavigation : Html.Html String
titleNavigation =
    div [ Html.Attributes.style [ ( "padding", toString padding ++ "px" ), ( "font-family", "sans-serif" ), ( "position", "absolute" ) ] ]
        [ Html.h1 [ Html.Attributes.style [ ( "margin-top", "0px" ), ( "font-size", "20px" ) ] ] [ text "Car Sales in Norway" ]
        , Example.navigation "Layout" exampleConfig
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


main : Program Never String String
main =
    Example.switchableViews exampleConfig (Shape.stack >> view)



{- {"additionalShots": ["stream-graph", "silhouette", "stacked-area"]} -}

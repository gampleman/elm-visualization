module NorwegianCarSales exposing (main)

{-| This example demonstates using different kinds of layouts for stacked graphs.
-}

import Axis
import Color exposing (Color)
import Example
import Html exposing (div, text)
import Html.Attributes
import List.Extra as List
import Path exposing (Path)
import SampleData
import Scale exposing (ContinuousScale, OrdinalScale, Scale)
import Scale.Color
import Shape exposing (StackConfig, StackResult)
import Time exposing (Month(..))
import Time.Extra exposing (Parts)
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (class, fill, fontFamily, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (fontSize)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Transform(..))


exampleConfig : List ( String, StackConfig String )
exampleConfig =
    [ ( "Stream Graph"
      , { data = samples
        , offset = Shape.stackOffsetWiggle
        , order = Shape.sortByInsideOut (Tuple.second >> List.sum)
        }
      )
    , ( "Silhouette"
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
    List.map Tuple.first samples
        |> List.reverse
        |> Scale.ordinal Scale.Color.category10


sampleColor : String -> Color
sampleColor label =
    Scale.convert colorScale label |> Maybe.withDefault Color.black


colors : List String -> List Color
colors labels =
    List.map sampleColor labels


w : Float
w =
    990


h : Float
h =
    504


padding : Float
padding =
    40


fromCalendarDate : Int -> Month -> Int -> Time.Posix
fromCalendarDate year month day =
    Time.Extra.partsToPosix Time.utc (Parts year month day 0 0 0 0)


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

        xScale : ContinuousScale Float
        xScale =
            -- map an index to screen space
            Scale.linear ( padding, w - padding - labelsWidth ) ( 0, toFloat size - 1 )

        yScale : ContinuousScale Float
        yScale =
            Scale.linear ( h - padding, padding ) extent
                |> Scale.nice 4

        xAxis : Svg msg
        xAxis =
            -- construct the time domain for display
            -- the data is per-month, so we have to pick a day
            -- to get the ticks to show up correctly, the upper bound needs to be Jan 2 (Jan 1 does not work).
            Scale.time Time.utc ( 0, w - padding * 2 - labelsWidth ) ( fromCalendarDate 2007 Jan 1, fromCalendarDate 2017 Jan 2 )
                |> Axis.bottom [ Axis.tickCount 1 ]

        yAxis : Svg msg
        yAxis =
            Axis.left [] yScale

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
            g [ transform [ Translate (w - padding - labelsWidth + 10) yPosition ] ]
                [ text_ [ fill (sampleColor label |> Fill) ] [ text label ] ]
    in
    div []
        [ titleNavigation
        , svg [ viewBox 0 0 w h ]
            [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
                [ xAxis ]
            , g [ class [ "series" ] ] paths
            , g [ fontFamily [ "sans-serif" ], fontSize 10 ]
                (List.map2 labelElement labels labelPositions)
            ]
        ]


titleNavigation : Html.Html String
titleNavigation =
    div [ Html.Attributes.style "padding" (String.fromFloat padding ++ "px"), Html.Attributes.style "font-family" "sans-serif", Html.Attributes.style "position" "absolute" ]
        [ Html.h1 [ Html.Attributes.style "margin-top" "0px", Html.Attributes.style "font-size" "20px" ] [ text "Car Sales in Norway" ]
        , Example.navigation "Layout" exampleConfig
        ]


{-| Renders one colored stream with given scaling
-}
renderStream : ( ContinuousScale Float, ContinuousScale Float ) -> Color -> List ( Float, Float ) -> Svg msg
renderStream scales color coords =
    Path.element (toArea scales coords) [ fill (Fill color) ]


{-| Create a svg path string that draws the area between two lines
-}
toArea : ( ContinuousScale Float, ContinuousScale Float ) -> List ( Float, Float ) -> Path
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


main =
    Example.switchableViews exampleConfig (Shape.stack >> view)



{- {"additionalShots": ["stream-graph", "silhouette", "stacked-area"]} -}

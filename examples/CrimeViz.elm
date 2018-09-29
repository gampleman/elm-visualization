module CrimeViz exposing (main)

{-| This module shows how to build a simple line and area chart using some of
the primitives provided in this library.
-}

import Color exposing (Color)
import Path exposing (Path)
import SampleData exposing (CrimeRate, crimeRates)
import Svg.Attributes exposing (fill, stroke)
import Time
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (class, dy, fontFamily, textAnchor, transform)
import TypedSvg.Attributes.InPx exposing (fontSize, height, strokeWidth, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Transform(..), em)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.List as List
import Visualization.Scale as Scale exposing (ContinuousScale, OrdinalScale)
import Visualization.Shape as Shape


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    60


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
    Scale.ordinal (List.map .label series) Scale.category10


colorString : String -> String
colorString =
    Scale.convert colorScale >> Maybe.withDefault Color.black >> Color.toCssString


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

        xScale : ContinuousScale
        xScale =
            model
                |> List.map (.year >> toFloat)
                |> List.extent
                |> Maybe.withDefault ( 1900, 1901 )
                |> (\a -> Scale.linear a ( 0, w - 2 * padding ))

        yScale : ContinuousScale
        yScale =
            model
                |> List.map (values >> List.maximum >> Maybe.withDefault 0)
                |> List.maximum
                |> Maybe.withDefault 0
                |> (\b -> ( 0, b ))
                |> (\a -> Scale.linear a ( h - 2 * padding, 0 ))
                |> (\a -> Scale.nice a 4)

        xAxis : Svg msg
        xAxis =
            Axis.axis { defaultOptions | orientation = Axis.Bottom, tickCount = 10 } xScale

        yAxis : Svg msg
        yAxis =
            Axis.axis { defaultOptions | orientation = Axis.Left, ticks = Just (values first) } yScale

        lineGenerator : ( Int, Int ) -> Maybe ( Float, Float )
        lineGenerator ( x, y ) =
            Just ( Scale.convert xScale (toFloat x), Scale.convert yScale (toFloat y) )

        line : (CrimeRate -> Int) -> Path
        line accessor =
            List.map (\i -> ( .year i, accessor i )) model
                |> List.map lineGenerator
                |> Shape.line Shape.monotoneInXCurve
    in
    svg [ width w, height h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis, text_ [ fontFamily [ "sans-serif" ], fontSize 10, x 5, y 5 ] [ text "Occurences" ] ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ]
            (List.map (\{ accessor, label } -> Path.element (line accessor) [ stroke (colorString label), strokeWidth 3, fill "none" ]) series)
        , g [ fontFamily [ "sans-serif" ], fontSize 10 ]
            (List.map
                (\{ accessor, label } ->
                    g [ transform [ Translate (w - padding + 10) (padding + Scale.convert yScale (toFloat (accessor last))) ] ]
                        [ text_ [ fill (colorString label) ] [ text label ] ]
                )
                series
            )
        , g [ transform [ Translate (w - padding) (padding + 20) ] ]
            [ text_ [ fontFamily [ "sans-serif" ], fontSize 20, textAnchor AnchorEnd ] [ text "Violent Crime in the US" ]
            , text_ [ fontFamily [ "sans-serif" ], fontSize 10, textAnchor AnchorEnd, dy (em 1) ] [ text "Source: fbi.gov" ]
            ]
        ]


main =
    view crimeRates

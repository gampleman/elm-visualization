module CrimeViz exposing (main)

{-| This module shows how to build a simple line and area chart using some of
the primitives provided in this library.
-}

import Date
import SampleData exposing (CrimeRate, crimeRates)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Visualization.Axis as Axis
import Visualization.List as List
import Visualization.Scale as Scale exposing (ContinuousScale)
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
      , color = "#843c39"
      , accessor = .murder
      }
    , { label = "Rape"
      , color = "#ad494a"
      , accessor = .rape
      }
    , { label = "Robbery"
      , color = "#d6616b"
      , accessor = .robbery
      }
    , { label = "Assault"
      , color = "#e7969c"
      , accessor = .assault
      }
    ]


view : List CrimeRate -> Svg msg
view model =
    let
        accessors : List (CrimeRate -> Int)
        accessors =
            List.map .accessor series

        last =
            List.reverse model |> List.head |> Maybe.withDefault (CrimeRate 0 0 0 0 0 0 0 0 0)

        first =
            List.head model |> Maybe.withDefault (CrimeRate 0 0 0 0 0 0 0 0 0)

        values i =
            List.map (\a -> a i) accessors

        xScale : ContinuousScale
        xScale =
            Scale.linear (List.map (.year >> toFloat) model |> List.extent |> Maybe.withDefault ( 1900, 1901 )) ( 0, w - 2 * padding )

        yScale : ContinuousScale
        yScale =
            Scale.nice (Scale.linear ( 0, List.map (values >> List.maximum >> Maybe.withDefault 0 >> toFloat) model |> List.maximum |> Maybe.withDefault 0 ) ( h - 2 * padding, 0 )) 4

        opts : Axis.Options a
        opts =
            Axis.defaultOptions

        xAxis : Svg msg
        xAxis =
            Axis.axis { opts | orientation = Axis.Bottom, tickCount = 10 } xScale

        yAxis : Svg msg
        yAxis =
            Axis.axis { opts | orientation = Axis.Left, ticks = Just (List.map toFloat (values first)) } yScale

        lineGenerator : ( Int, Int ) -> Maybe ( Float, Float )
        lineGenerator ( x, y ) =
            Just ( Scale.convert xScale (toFloat x), Scale.convert yScale (toFloat y) )

        line : (CrimeRate -> Int) -> String
        line accessor =
            List.map (\i -> ( .year i, accessor i )) model
                |> List.map lineGenerator
                |> Shape.line Shape.monotoneInXCurve
    in
        svg [ width (toString w ++ "px"), height (toString h ++ "px") ]
            [ g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString (h - padding) ++ ")") ]
                [ xAxis ]
            , g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString padding ++ ")") ]
                [ yAxis, text_ [ fontFamily "sans-serif", fontSize "10", x "5", y "5" ] [ text "Occurences" ] ]
            , g [ transform ("translate(" ++ toString padding ++ ", " ++ toString padding ++ ")"), class "series" ]
                (List.map (\{ accessor, color } -> Svg.path [ d (line accessor), stroke color, strokeWidth "3px", fill "none" ] []) series)
            , g [ fontFamily "sans-serif", fontSize "10" ]
                (List.map
                    (\{ accessor, color, label } ->
                        g [ transform ("translate(" ++ toString (w - padding + 10) ++ ", " ++ toString (padding + Scale.convert yScale (toFloat (accessor last))) ++ ")") ]
                            [ text_ [ fill color ] [ text label ] ]
                    )
                    series
                )
            , g [ transform ("translate(" ++ toString (w - padding) ++ ", " ++ toString (padding + 20) ++ ")") ]
                [ text_ [ fontFamily "sans-serif", fontSize "20", textAnchor "end" ] [ text "Violent Crime in the US" ]
                , text_ [ fontFamily "sans-serif", fontSize "10", textAnchor "end", dy "1em" ] [ text "Source: fbi.gov" ]
                ]
            ]


main =
    view crimeRates

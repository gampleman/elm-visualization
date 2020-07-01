module CustomPieChart exposing (main)

{-| An interactive example showing the effect of various options on pie generators.

@category Reference

-}

import Array exposing (Array)
import Color exposing (Color)
import Example
import Html exposing (Html, br, div, h2, input, label)
import Html.Attributes as Attr exposing (step, style, type_, value)
import Html.Events exposing (onInput)
import Path
import Shape exposing (defaultPieConfig)
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (dy, fill, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Paint(..), Transform(..), em)


w : Float
w =
    990


h : Float
h =
    504


colors : Array Color
colors =
    Array.fromList
        [ Color.rgb255 152 171 198
        , Color.rgb255 138 137 166
        , Color.rgb255 123 104 136
        , Color.rgb255 107 72 107
        , Color.rgb255 159 92 85
        , Color.rgb255 208 116 60
        , Color.rgb255 255 96 0
        ]


radius : Float
radius =
    min w h / 2


type alias ChartConfig =
    { outerRadius : Float
    , innerRadius : Float
    , padAngle : Float
    , cornerRadius : Float
    , labelPosition : Float
    }


configuration : Example.Configuration ChartConfig
configuration =
    Example.configuration
        { outerRadius = 210
        , innerRadius = 200
        , padAngle = 0.02
        , cornerRadius = 20
        , labelPosition = 230
        }
        [ Example.floatSlider "Outer Radius" .outerRadius (\v m -> { m | outerRadius = v }) 0 radius
        , Example.floatSlider "Inner Radius" .innerRadius (\v m -> { m | innerRadius = v }) 0 radius
        , Example.floatSlider "Pad Angle" .padAngle (\v m -> { m | padAngle = v }) 0 0.8
        , Example.floatSlider "Corner Radius" .cornerRadius (\v m -> { m | cornerRadius = v }) 0 20
        , Example.floatSlider "Label Position" .labelPosition (\v m -> { m | labelPosition = v }) 0 radius
        ]


drawChart : ChartConfig -> List ( String, Float ) -> Svg msg
drawChart config model =
    let
        pieData =
            model
                |> List.map Tuple.second
                |> Shape.pie
                    { defaultPieConfig
                        | innerRadius = config.innerRadius
                        , outerRadius = config.outerRadius
                        , padAngle = config.padAngle
                        , cornerRadius = config.cornerRadius
                        , sortingFn = \_ _ -> EQ
                    }

        makeSlice index datum =
            Path.element (Shape.arc datum) [ fill <| Paint <| Maybe.withDefault Color.black <| Array.get index colors, stroke <| Paint Color.white ]

        makeLabel slice ( label, value ) =
            let
                ( x, y ) =
                    Shape.centroid { slice | innerRadius = config.labelPosition, outerRadius = config.labelPosition }
            in
            text_
                [ transform [ Translate x y ]
                , dy (em 0.35)
                , textAnchor AnchorMiddle
                ]
                [ text label ]
    in
    svg [ width (radius * 2), height (radius * 2) ]
        [ g [ transform [ Translate radius radius ] ]
            [ g [] <| List.indexedMap makeSlice pieData
            , g [] <| List.map2 makeLabel pieData model
            ]
        ]


view : ChartConfig -> Html (Example.ConfigMsg ChartConfig)
view model =
    div [ style "display" "flex", style "justify-content" "space-around" ]
        [ drawChart model data
        , Example.verticalPanel "Pie Configuration" configuration model
        ]


data : List ( String, Float )
data =
    [ ( "<5", 2704659 )
    , ( "5-13", 4499890 )
    , ( "14-17", 2159981 )
    , ( "18-24", 3853788 )
    , ( "25-44", 14106543 )
    , ( "45-64", 8819342 )
    , ( "≥65", 612463 )
    ]


main =
    Example.configurable configuration view

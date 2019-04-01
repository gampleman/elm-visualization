module CustomPieChart exposing (main)

{-| An interactive example showing the effect of various options on pie generators.
-}

import Array exposing (Array)
import Browser
import Color exposing (Color)
import Html exposing (Html, br, div, h2, input, label)
import Html.Attributes as Attr exposing (step, style, type_, value)
import Html.Events exposing (onInput)
import Path
import Shape exposing (defaultPieConfig)
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (dy, fill, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Transform(..), em)


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


type Msg
    = UpdateOuterRadius String
    | UpdateInnerRadius String
    | UpdatePadAngle String
    | UpdateCornerRadius String
    | UpdateLabelPosition String


type alias Model =
    { config : ChartConfig
    , data : List ( String, Float )
    }


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
            Path.element (Shape.arc datum) [ fill <| Fill <| Maybe.withDefault Color.black <| Array.get index colors, stroke Color.white ]

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


update : Msg -> Model -> Model
update msg model =
    let
        config =
            model.config
    in
    case msg of
        UpdateOuterRadius amount ->
            { model | config = { config | outerRadius = Maybe.withDefault 0 <| String.toFloat amount } }

        UpdateInnerRadius amount ->
            { model | config = { config | innerRadius = Maybe.withDefault 0 <| String.toFloat amount } }

        UpdatePadAngle amount ->
            { model | config = { config | padAngle = Maybe.withDefault 0 <| String.toFloat amount } }

        UpdateCornerRadius amount ->
            { model | config = { config | cornerRadius = Maybe.withDefault 0 <| String.toFloat amount } }

        UpdateLabelPosition amount ->
            { model | config = { config | labelPosition = Maybe.withDefault 0 <| String.toFloat amount } }


view : Model -> Html Msg
view model =
    div [ style "display" "flex", style "justify-content" "space-around" ]
        [ drawChart model.config model.data
        , div []
            [ h2 [] [ text "Pie Configuration" ]
            , label [] [ text "Outer Radius" ]
            , input
                [ type_ "range"
                , onInput UpdateOuterRadius
                , value (String.fromFloat model.config.outerRadius)
                , Attr.min "0"
                , Attr.max (String.fromFloat radius)
                ]
                []
            , text (String.fromFloat model.config.outerRadius)
            , br [] []
            , label [] [ text "Inner Radius" ]
            , input
                [ type_ "range"
                , onInput UpdateInnerRadius
                , value (String.fromFloat model.config.innerRadius)
                , Attr.min "0"
                , Attr.max (String.fromFloat radius)
                ]
                []
            , text (String.fromFloat model.config.innerRadius)
            , br [] []
            , label [] [ text "Pad Angle" ]
            , input
                [ type_ "range"
                , onInput UpdatePadAngle
                , value (String.fromFloat model.config.padAngle)
                , Attr.min "0"
                , Attr.max "0.8"
                , step "0.01"
                ]
                []
            , text (String.fromFloat model.config.padAngle)
            , br [] []
            , label [] [ text "Corner Radius" ]
            , input
                [ type_ "range"
                , onInput UpdateCornerRadius
                , value (String.fromFloat model.config.cornerRadius)
                , Attr.min "0"
                , Attr.max "20"
                , step "0.25"
                ]
                []
            , text (String.fromFloat model.config.cornerRadius)
            , br [] []
            , label [] [ text "Label Position" ]
            , input
                [ type_ "range"
                , onInput UpdateLabelPosition
                , value (String.fromFloat model.config.labelPosition)
                , Attr.min "0"
                , Attr.max (String.fromFloat radius)
                ]
                []
            , text (String.fromFloat model.config.labelPosition)
            , br [] []
            ]
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


init : { config : ChartConfig, data : List ( String, Float ) }
init =
    { config =
        { outerRadius = 210
        , innerRadius = 200
        , padAngle = 0.02
        , cornerRadius = 20
        , labelPosition = 230
        }
    , data = data
    }


main : Program () { config : ChartConfig, data : List ( String, Float ) } Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }

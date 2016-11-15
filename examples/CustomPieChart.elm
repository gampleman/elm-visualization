module CustomPieChart exposing (main)

{-| An example showing how to render a basic pie chart.
-}

import Visualization.Shape as Shape exposing (defaultPieConfig)
import Array exposing (Array)
import Svg exposing (Svg, svg, g, path, text_, text)
import Svg.Attributes exposing (transform, d, style, dy, width, height, textAnchor)
import Html exposing (Html, div, input, label, br, h2)
import Html.Attributes exposing (type_, value, step)
import Html.Events exposing (onInput)
import String


screenWidth : Float
screenWidth =
    990


screenHeight : Float
screenHeight =
    504


colors : Array String
colors =
    Array.fromList [ "#98abc5", "#8a89a6", "#7b6888", "#6b486b", "#a05d56", "#d0743c", "#ff8c00" ]


radius : Float
radius =
    min screenWidth screenHeight / 2


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
            path [ d (Shape.arc datum), style ("fill:" ++ (Maybe.withDefault "#000" <| Array.get index colors) ++ "; stroke: #fff;") ] []

        makeLabel slice ( label, value ) =
            text_
                [ transform ("translate" ++ toString (Shape.centroid { slice | innerRadius = config.labelPosition, outerRadius = config.labelPosition }))
                , dy ".35em"
                , textAnchor "middle"
                ]
                [ text label ]
    in
        svg [ width (toString (radius * 2) ++ "px"), height (toString (radius * 2) ++ "px") ]
            [ g [ transform ("translate(" ++ toString radius ++ "," ++ toString radius ++ ")") ]
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
                { model | config = { config | outerRadius = Result.withDefault 0 <| String.toFloat amount } }

            UpdateInnerRadius amount ->
                { model | config = { config | innerRadius = Result.withDefault 0 <| String.toFloat amount } }

            UpdatePadAngle amount ->
                { model | config = { config | padAngle = Result.withDefault 0 <| String.toFloat amount } }

            UpdateCornerRadius amount ->
                { model | config = { config | cornerRadius = Result.withDefault 0 <| String.toFloat amount } }

            UpdateLabelPosition amount ->
                { model | config = { config | labelPosition = Result.withDefault 0 <| String.toFloat amount } }


view : Model -> Html Msg
view model =
    div [ style "display: flex;  justify-content: space-around" ]
        [ drawChart model.config model.data
        , div []
            [ h2 [] [ text "Pie Configuration" ]
            , label [] [ text "Outer Radius" ]
            , input [ type_ "range", onInput UpdateOuterRadius, value (toString model.config.outerRadius), Html.Attributes.min "0", Html.Attributes.max (toString radius) ] []
            , text (toString model.config.outerRadius)
            , br [] []
            , label [] [ text "Inner Radius" ]
            , input [ type_ "range", onInput UpdateInnerRadius, value (toString model.config.innerRadius), Html.Attributes.min "0", Html.Attributes.max (toString radius) ] []
            , text (toString model.config.innerRadius)
            , br [] []
            , label [] [ text "Pad Angle" ]
            , input [ type_ "range", onInput UpdatePadAngle, value (toString model.config.padAngle), Html.Attributes.min "0", Html.Attributes.max "0.8", step "0.01" ] []
            , text (toString model.config.padAngle)
            , br [] []
            , label [] [ text "Corner Radius" ]
            , input [ type_ "range", onInput UpdateCornerRadius, value (toString model.config.cornerRadius), Html.Attributes.min "0", Html.Attributes.max "20", step "0.25" ] []
            , text (toString model.config.cornerRadius)
            , br [] []
            , label [] [ text "Label Position" ]
            , input [ type_ "range", onInput UpdateLabelPosition, value (toString model.config.labelPosition), Html.Attributes.min "0", Html.Attributes.max (toString radius) ] []
            , text (toString model.config.labelPosition)
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


model : { config : ChartConfig, data : List ( String, Float ) }
model =
    { config =
        { outerRadius = 210
        , innerRadius = 200
        , padAngle = 0.02
        , cornerRadius = 20
        , labelPosition = 230
        }
    , data = data
    }


main : Program Never { config : ChartConfig, data : List ( String, Float ) } Msg
main =
    Html.beginnerProgram
        { model = model
        , update = update
        , view = view
        }

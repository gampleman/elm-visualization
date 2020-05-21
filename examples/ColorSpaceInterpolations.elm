module ColorSpaceInterpolations exposing (main)

{-| This module shows how to build some simple colour space palettes.
-}

import Browser
import Color exposing (Color, rgb255)
import Hex
import Html exposing (Html, div)
import Html.Attributes exposing (class, for, href, id, selected, style, type_, value)
import Html.Events exposing (onInput)
import Interpolation exposing (Interpolator)
import Scale exposing (QuantizeScale)


css : String
css =
    """
h1 {
    font-size: 24px;
}

body {
    font-family: Sans-Serif;
}

.wrapper {
    margin: 25px;
    width: 940px;
    position: relative;
}

.palette {
    display: flex;
    width: 100%;
    margin: 2px 0 10px;
}

.palette div {
    height: 40px;
    flex: 1;
}

.controls {
    margin-bottom : 0;
    display: flex;
}

.controls div {
    margin : 0 20px 10px;
}

.controls input,
.controls select {
    margin-right : 10px;
}
"""


type alias Model =
    { fromColorValue : String
    , toColorValue : String
    , count : Int
    }


init : Model
init =
    { fromColorValue = "#00ff00"
    , toColorValue = "#ff0200"
    , count = 50
    }


type Msg
    = FromColorInput String
    | ToColorInput String
    | Count String


update : Msg -> Model -> Model
update msg model =
    case msg of
        FromColorInput val ->
            { model | fromColorValue = val }

        ToColorInput val ->
            { model | toColorValue = val }

        Count val ->
            { model | count = val |> String.toInt |> Maybe.withDefault 0 }


palette : Model -> (Color -> Color -> Interpolator Color) -> Html msg
palette model colorSpaceInterpolator =
    div [ class "palette" ]
        (Interpolation.samples model.count
            (colorSpaceInterpolator (hexToColor model.fromColorValue) (hexToColor model.toColorValue))
            |> List.map
                (\color ->
                    Html.div [ style "background-color" (Color.toCssString color) ] []
                )
        )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.node "style" [] [ Html.text css ]
        , Html.div
            [ class "wrapper" ]
            [ div [] [ Html.h1 [] [ Html.text "Color Space Interpolations" ] ]
            , controls model
            , div [] [ Html.a [ href "https://en.wikipedia.org/wiki/CIELAB_color_space" ] [ Html.text "lab" ] ]
            , palette model Interpolation.lab
            , div [] [ Html.text "rgb" ]
            , palette model Interpolation.rgb
            , div [] [ Html.a [ href "https://en.wikipedia.org/wiki/HCL_color_space" ] [ Html.text "hcl" ] ]
            , palette model Interpolation.hcl
            , div [] [ Html.a [ href "https://en.wikipedia.org/wiki/HSL_and_HSV" ] [ Html.text "hsl" ] ]
            , palette model Interpolation.hsl
            , div [] [ Html.a [ href "https://en.wikipedia.org/wiki/HCL_color_space" ] [ Html.text "hclLong" ] ]
            , palette model Interpolation.hclLong
            , div [] [ Html.a [ href "https://en.wikipedia.org/wiki/HSL_and_HSV" ] [ Html.text "hslLong" ] ]
            , palette model Interpolation.hslLong
            ]
        ]


controls : Model -> Html Msg
controls model =
    div [ class "controls" ]
        [ fromColorInput model
        , toColorInput model
        , selectNumberOfColors model
        ]


fromColorInput : Model -> Html Msg
fromColorInput model =
    div []
        [ Html.input [ id "from", type_ "color", onInput FromColorInput, value model.fromColorValue ] []
        , Html.label [ for "from" ] [ Html.text "Start color" ]
        ]


toColorInput : Model -> Html Msg
toColorInput model =
    div []
        [ Html.input [ id "to", type_ "color", onInput ToColorInput, value model.toColorValue ] []
        , Html.label [ for "to" ] [ Html.text "End color" ]
        ]


selectNumberOfColors : Model -> Html Msg
selectNumberOfColors model =
    div []
        [ Html.select [ id "number-of-colors", onInput Count ]
            ([ "2", "3", "4", "5", "6", "7", "8", "9", "10", "15", "20", "25", "30", "35", "40", "45", "50", "100" ]
                |> List.map
                    (\v ->
                        let
                            isSelected =
                                if model.count |> String.fromInt |> (==) v then
                                    True

                                else
                                    False
                        in
                        Html.option [ value v, selected isSelected ] [ Html.text v ]
                    )
            )
        , Html.label [ for "number-of-colors" ] [ Html.text "Number of colors" ]
        ]


main =
    Browser.sandbox { init = init, update = update, view = view }


{-| Hexadecimal color string to Color
-}
hexToColor : String -> Color
hexToColor hex =
    hex
        |> String.dropLeft 1
        |> (\s ->
                let
                    r =
                        String.slice 0 2 s
                            |> Hex.fromString
                            |> Result.withDefault 0

                    g =
                        String.slice 2 4 s
                            |> Hex.fromString
                            |> Result.withDefault 0

                    b =
                        String.slice 4 6 s
                            |> Hex.fromString
                            |> Result.withDefault 0
                in
                rgb255 r g b
           )

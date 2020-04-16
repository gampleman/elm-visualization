module ColorMaps exposing (main)

{-| This module shows how to build some simple colour space palettes.
-}

import Browser
import Color exposing (Color, rgb255)
import Hex
import Html exposing (Html, div)
import Html.Attributes exposing (class, for, href, id, selected, style, type_, value)
import Html.Events exposing (onInput)
import Interpolation exposing (Interpolator)
import Scale.Color exposing (turboInterpolator)


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


turboColorMap : Html msg
turboColorMap =
    div [ class "palette" ]
        (List.range 1 101
            |> List.map (\v -> toFloat v / 100)
            |> List.map turboInterpolator
            |> List.map
                (\color ->
                    Html.div [ style "background-color" (Color.toCssString color) ] []
                )
        )


view : Html msg
view =
    Html.div []
        [ Html.node "style" [] [ Html.text css ]
        , Html.div
            [ class "wrapper" ]
            [ div [] [ Html.h1 [] [ Html.text "Color Maps" ] ]
            , div []
                [ Html.a
                    [ href "https://ai.googleblog.com/2019/08/turbo-improved-rainbow-colormap-for.html"
                    ]
                    [ Html.text "turbo" ]
                ]
            , turboColorMap
            ]
        ]


main =
    view

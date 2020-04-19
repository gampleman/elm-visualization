module ColorMaps exposing (main)

{-| This module shows how to build some simple colour space palettes.
-}

import Array
import Browser
import Color exposing (Color, rgb255)
import Hex
import Html exposing (Html, div)
import Html.Attributes exposing (class, for, href, id, selected, style, type_, value)
import Html.Events exposing (onInput)
import Interpolation exposing (Interpolator)
import Scale.Color
    exposing
        ( blueGreenInterpolator
        , blueOrangeInterpolator
        , bluePurpleInterpolator
        , bluesInterpolator
        , brownBlueGreenInterpolator
        , greenBlueInterpolator
        , greensInterpolator
        , greysInterpolator
        , hexToColor
        , orangeRedInterpolator
        , orangesInterpolator
        , pinkYellowGreenInterpolator
        , purpleBlueGreenInterpolator
        , purpleBlueInterpolator
        , purpleGreenInterpolator
        , purpleOrangeInterpolator
        , purpleRedInterpolator
        , purplesInterpolator
        , rainbowInterpolator
        , redBlueInterpolator
        , redGreyInterpolator
        , redPurpleInterpolator
        , redYellowBlueInterpolator
        , redYellowGreenInterpolator
        , redsInterpolator
        , sinebowInterpolator
        , spectralInterpolator
        , turboInterpolator
        , yellowGreenBlueInterpolator
        , yellowGreenInterpolator
        , yellowOrangeBrownInterpolator
        , yellowOrangeRedInterpolator
        )


css : String
css =
    """
h1 {
    font-size: 24px;
}

h2 {
    font-size: 20px;
    font-weight: normal;
    margin: 30px 0 20px 0;
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
    height: 25px;
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
    div []
        [ div []
            [ Html.a
                [ href "https://ai.googleblog.com/2019/08/turbo-improved-rainbow-colormap-for.html"
                ]
                [ Html.text "Turbo" ]
            ]
        , div [ class "palette" ] (interpolation turboInterpolator)
        ]


cyclic : List (Html msg)
cyclic =
    [ ( "rainbow", rainbowInterpolator )
    , ( "sinebow", sinebowInterpolator )
    ]
        |> List.map
            (\( title, interpolator ) ->
                div []
                    [ div []
                        [ Html.text title ]
                    , div [ class "palette" ] (interpolation interpolator)
                    ]
            )
        |> (::) turboColorMap


sequentialSingleHue : List (Html msg)
sequentialSingleHue =
    [ ( "blue", bluesInterpolator )
    , ( "green", greensInterpolator )
    , ( "grey", greysInterpolator )
    , ( "orange", orangesInterpolator )
    , ( "purple", purplesInterpolator )
    , ( "red", redsInterpolator )
    ]
        |> List.map
            (\( title, interpolator ) ->
                div []
                    [ div []
                        [ Html.text title ]
                    , div [ class "palette" ] (interpolation interpolator)
                    ]
            )


sequentialMultiHue : List (Html msg)
sequentialMultiHue =
    [ ( "blue-green", blueGreenInterpolator )
    , ( "blue-purple", bluePurpleInterpolator )
    , ( "green-blue", greenBlueInterpolator )
    , ( "orange-red", orangeRedInterpolator )
    , ( "purple-blue", purpleBlueInterpolator )
    , ( "purple-blue-green", purpleBlueGreenInterpolator )
    , ( "purple-red", purpleRedInterpolator )
    , ( "red-purple", redPurpleInterpolator )
    , ( "yellow-green", yellowGreenInterpolator )
    , ( "yellow-orange-brown", yellowOrangeBrownInterpolator )
    , ( "yellow-orange-red", yellowOrangeRedInterpolator )
    ]
        |> List.map
            (\( title, interpolator ) ->
                div []
                    [ div []
                        [ Html.text title ]
                    , div [ class "palette" ] (interpolation interpolator)
                    ]
            )


diverging : List (Html msg)
diverging =
    [ ( "blue-orange", blueOrangeInterpolator )
    , ( "brown-blue-green", brownBlueGreenInterpolator )
    , ( "purple-green", purpleGreenInterpolator )
    , ( "purple-orange", purpleOrangeInterpolator )
    , ( "red-blue", redBlueInterpolator )
    , ( "red-grey", redGreyInterpolator )
    , ( "yellow-green-blue", yellowGreenBlueInterpolator )
    , ( "red-yellow-blue", redYellowBlueInterpolator )
    , ( "red-yellow-green", redYellowGreenInterpolator )
    , ( "pink-yellow-green", pinkYellowGreenInterpolator )
    , ( "spectral", spectralInterpolator )
    ]
        |> List.map
            (\( title, interpolator ) ->
                div []
                    [ div []
                        [ Html.text title ]
                    , div [ class "palette" ] (interpolation interpolator)
                    ]
            )


interpolation : Interpolator Color -> List (Html msg)
interpolation interpolator =
    List.range 1 41
        |> List.map (\v -> toFloat v / 40)
        |> List.map interpolator
        |> List.map
            (\color ->
                Html.div [ style "background-color" (Color.toCssString color) ] []
            )


view : Html msg
view =
    Html.div []
        [ Html.node "style" [] [ Html.text css ]
        , Html.div
            [ class "wrapper" ]
            [ div [] [ Html.h1 [] [ Html.text "Color Maps" ] ]
            , div []
                [ div []
                    (Html.h2 [] [ Html.text "Sequential Single-Hue" ]
                        :: sequentialSingleHue
                    )
                ]
            , div []
                [ div []
                    (Html.h2 [] [ Html.text "Sequential Multi-Hue" ]
                        :: sequentialMultiHue
                    )
                ]
            , div []
                [ div []
                    (Html.h2 [] [ Html.text "Diverging" ]
                        :: diverging
                    )
                ]
            , div []
                [ div []
                    (Html.h2 [] [ Html.text "Cyclic" ]
                        :: cyclic
                    )
                ]
            ]
        ]


main =
    view

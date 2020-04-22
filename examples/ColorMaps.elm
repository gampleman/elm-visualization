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
        ( accent
        , blueGreenInterpolator
        , blueOrangeInterpolator
        , bluePurpleInterpolator
        , bluesInterpolator
        , brownBlueGreenInterpolator
        , brownsInterpolator
        , carbonDiverging1Interpolator
        , carbonDiverging2Interpolator
        , category10
        , colorblind
        , goldGreensInterpolator
        , goldOrangeInterpolator
        , goldRedInterpolator
        , greenBlueInterpolator
        , greensInterpolator
        , greysInterpolator
        , hexToColor
        , lightGreyRedInterpolator
        , lightGreyTealInterpolator
        , lightMultiInterpolator
        , lightOrangeInterpolator
        , orangeRedInterpolator
        , orangesInterpolator
        , paired
        , pastel1
        , pastel2
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
        , set1
        , set2
        , sinebowInterpolator
        , spectralInterpolator
        , tableau10
        , tealBluesInterpolator
        , tealInterpolator
        , turboInterpolator
        , warmGreysInterpolator
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
    , ( "light-orange", lightOrangeInterpolator )
    , ( "purple", purplesInterpolator )
    , ( "red", redsInterpolator )
    , ( "browns", brownsInterpolator )
    , ( "teal", tealInterpolator )
    , ( "warm grey", warmGreysInterpolator )
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
    , ( "teal-blues", tealBluesInterpolator )
    , ( "gold-green", goldGreensInterpolator )
    , ( "gold-orange", goldOrangeInterpolator )
    , ( "gold-red", goldRedInterpolator )
    , ( "light-grey-red", lightGreyRedInterpolator )
    , ( "light-grey-teal", lightGreyTealInterpolator )
    , ( "light-multi", lightMultiInterpolator )
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
    [ ( "carbon palette1", carbonDiverging1Interpolator )
    , ( "carbon palette2", carbonDiverging2Interpolator )
    , ( "blue-orange", blueOrangeInterpolator )
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


categorical : List (Html msg)
categorical =
    [ ( "category10", category10 )
    , ( "accent", accent )
    , ( "tableau10", tableau10 )
    , ( "paired", paired )
    , ( "pastel1", pastel1 )
    , ( "pastel2", pastel2 )
    , ( "colorblind", colorblind )
    , ( "set1", set1 )
    , ( "set2", set2 )
    ]
        |> List.map
            (\( title, colors ) ->
                div []
                    [ div []
                        [ Html.text title ]
                    , div [ class "palette" ]
                        (colors
                            |> List.map
                                (\color ->
                                    Html.div [ style "background-color" (Color.toCssString color) ] []
                                )
                        )
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
                    (Html.h2 [] [ Html.text "Categorical" ]
                        :: categorical
                    )
                ]
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

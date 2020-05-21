module ColorMaps exposing (main)

{-| This module shows the color schemes available in the Scale.Color module
-}

import Array
import Browser
import Color exposing (Color, rgb255)
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
        , carbonAlert
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
    width: 500px;
    position: relative;
}

.palette {
    display: flex;
    width: 100%;
    margin: 2px 0 10px;
}

.palette div {
    height: 30px;
    flex: 1;
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
    [ ( "blues", bluesInterpolator )
    , ( "greens", greensInterpolator )
    , ( "greys", greysInterpolator )
    , ( "oranges", orangesInterpolator )
    , ( "light-oranges", lightOrangeInterpolator )
    , ( "purples", purplesInterpolator )
    , ( "reds", redsInterpolator )
    , ( "browns", brownsInterpolator )
    , ( "teals", tealInterpolator )
    , ( "warm greys", warmGreysInterpolator )
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
    [ ( "blue-greens", blueGreenInterpolator )
    , ( "blue-purples", bluePurpleInterpolator )
    , ( "green-blues", greenBlueInterpolator )
    , ( "orange-reds", orangeRedInterpolator )
    , ( "purple-blues", purpleBlueInterpolator )
    , ( "purple-blue-greens", purpleBlueGreenInterpolator )
    , ( "purple-reds", purpleRedInterpolator )
    , ( "red-purples", redPurpleInterpolator )
    , ( "yellow-greens", yellowGreenInterpolator )
    , ( "yellow-orange-browns", yellowOrangeBrownInterpolator )
    , ( "yellow-orange-reds", yellowOrangeRedInterpolator )
    , ( "teal-bluess", tealBluesInterpolator )
    , ( "gold-greens", goldGreensInterpolator )
    , ( "gold-oranges", goldOrangeInterpolator )
    , ( "gold-reds", goldRedInterpolator )
    , ( "light-grey-reds", lightGreyRedInterpolator )
    , ( "light-grey-teals", lightGreyTealInterpolator )
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
    , ( "blue-oranges", blueOrangeInterpolator )
    , ( "brown-blue-greens", brownBlueGreenInterpolator )
    , ( "purple-greens", purpleGreenInterpolator )
    , ( "purple-oranges", purpleOrangeInterpolator )
    , ( "red-blues", redBlueInterpolator )
    , ( "red-greys", redGreyInterpolator )
    , ( "yellow-green-blues", yellowGreenBlueInterpolator )
    , ( "red-yellow-blues", redYellowBlueInterpolator )
    , ( "red-yellow-greens", redYellowGreenInterpolator )
    , ( "pink-yellow-greens", pinkYellowGreenInterpolator )
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
    , ( "tableau10", tableau10 )
    , ( "paired", paired )
    , ( "set1", set1 )
    , ( "set2", set2 )
    , ( "accent", accent )
    , ( "pastel1", pastel1 )
    , ( "pastel2", pastel2 )
    , ( "colorblind", colorblind )
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
    List.range 1 61
        |> List.map (\v -> toFloat v / 60)
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

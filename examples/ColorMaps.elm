module ColorMaps exposing (main)

{-| This module shows the color schemes available in the Scale.Color module

@category Reference

-}

import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes exposing (style, title)
import Interpolation exposing (Interpolator)
import Scale.Color


pallete : String -> List ( String, List (Html msg) ) -> Html msg
pallete title items =
    Html.section [ style "display" "flex", style "align-items" "flex-start", style "margin-bottom" "30px", style "padding-right" "20px" ]
        [ Html.h3 [ style "position" "sticky", style "top" "10px", style "width" "250px" ] [ Html.text title ]
        , items
            |> List.map
                (\( name, colors ) ->
                    Html.div []
                        [ Html.h5 [ style "margin-bottom" "4px", style "margin-top" "10px", style "font-weight" "normal" ] [ Html.text name ]
                        , Html.div [ style "display" "flex" ] colors
                        ]
                )
            |> Html.div [ style "flex-grow" "1" ]
        ]


interpolatorPallete : String -> List ( String, Interpolator Color ) -> Html msg
interpolatorPallete title items =
    pallete title (List.map (Tuple.mapSecond interpolation) items)


collectionPallete : String -> List ( String, List Color ) -> Html msg
collectionPallete title items =
    pallete title (List.map (Tuple.mapSecond (List.map swatch)) items)


swatch : Color -> Html msg
swatch color =
    Html.div [ style "background-color" (Color.toCssString color), title (Color.toCssString color), style "height" "30px", style "flex-grow" "1" ] []


interpolation : Interpolator Color -> List (Html msg)
interpolation interpolator =
    List.range 1 61
        |> List.map (\v -> toFloat v / 60)
        |> List.map interpolator
        |> List.map swatch


view : Html msg
view =
    Html.div [ style "height" "100vh", style "overflow-y" "auto", style "font-family" "-apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif" ]
        [ collectionPallete "Categorical"
            [ ( "category10", Scale.Color.category10 )
            , ( "tableau10", Scale.Color.tableau10 )
            , ( "paired", Scale.Color.paired )
            , ( "set1", Scale.Color.set1 )
            , ( "set2", Scale.Color.set2 )
            , ( "accent", Scale.Color.accent )
            , ( "pastel1", Scale.Color.pastel1 )
            , ( "pastel2", Scale.Color.pastel2 )
            , ( "colorblind", Scale.Color.colorblind )
            ]
        , interpolatorPallete "Sequential Single-Hue"
            [ ( "blues", Scale.Color.bluesInterpolator )
            , ( "greens", Scale.Color.greensInterpolator )
            , ( "greys", Scale.Color.greysInterpolator )
            , ( "oranges", Scale.Color.orangesInterpolator )
            , ( "light-oranges", Scale.Color.lightOrangeInterpolator )
            , ( "purples", Scale.Color.purplesInterpolator )
            , ( "reds", Scale.Color.redsInterpolator )
            , ( "browns", Scale.Color.brownsInterpolator )
            , ( "teals", Scale.Color.tealInterpolator )
            , ( "warm greys", Scale.Color.warmGreysInterpolator )
            ]
        , interpolatorPallete "Sequential Multi-Hue"
            [ ( "blue-greens", Scale.Color.blueGreenInterpolator )
            , ( "blue-purples", Scale.Color.bluePurpleInterpolator )
            , ( "green-blues", Scale.Color.greenBlueInterpolator )
            , ( "orange-reds", Scale.Color.orangeRedInterpolator )
            , ( "purple-blues", Scale.Color.purpleBlueInterpolator )
            , ( "purple-blue-greens", Scale.Color.purpleBlueGreenInterpolator )
            , ( "purple-reds", Scale.Color.purpleRedInterpolator )
            , ( "red-purples", Scale.Color.redPurpleInterpolator )
            , ( "yellow-greens", Scale.Color.yellowGreenInterpolator )
            , ( "yellow-orange-browns", Scale.Color.yellowOrangeBrownInterpolator )
            , ( "yellow-orange-reds", Scale.Color.yellowOrangeRedInterpolator )
            , ( "teal-bluess", Scale.Color.tealBluesInterpolator )
            , ( "gold-greens", Scale.Color.goldGreensInterpolator )
            , ( "gold-oranges", Scale.Color.goldOrangeInterpolator )
            , ( "gold-reds", Scale.Color.goldRedInterpolator )
            , ( "light-grey-reds", Scale.Color.lightGreyRedInterpolator )
            , ( "light-grey-teals", Scale.Color.lightGreyTealInterpolator )
            , ( "light-multi", Scale.Color.lightMultiInterpolator )
            ]
        , interpolatorPallete "Diverging"
            [ ( "carbon palette1", Scale.Color.carbonDiverging1Interpolator )
            , ( "carbon palette2", Scale.Color.carbonDiverging2Interpolator )
            , ( "blue-oranges", Scale.Color.blueOrangeInterpolator )
            , ( "brown-blue-greens", Scale.Color.brownBlueGreenInterpolator )
            , ( "purple-greens", Scale.Color.purpleGreenInterpolator )
            , ( "purple-oranges", Scale.Color.purpleOrangeInterpolator )
            , ( "red-blues", Scale.Color.redBlueInterpolator )
            , ( "red-greys", Scale.Color.redGreyInterpolator )
            , ( "yellow-green-blues", Scale.Color.yellowGreenBlueInterpolator )
            , ( "red-yellow-blues", Scale.Color.redYellowBlueInterpolator )
            , ( "red-yellow-greens", Scale.Color.redYellowGreenInterpolator )
            , ( "pink-yellow-greens", Scale.Color.pinkYellowGreenInterpolator )
            , ( "spectral", Scale.Color.spectralInterpolator )
            ]
        , interpolatorPallete "Cyclic"
            [ ( "rainbow", Scale.Color.rainbowInterpolator )
            , ( "sinebow", Scale.Color.sinebowInterpolator )
            , ( "turbo", Scale.Color.turboInterpolator )
            ]
        ]


main : Html msg
main =
    view

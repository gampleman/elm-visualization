module ColorSpaceInterpolations exposing (Model, main)

{-| This module shows how to build some simple colour space palettes.

@category Reference

-}

import Color exposing (Color)
import Example
import Html exposing (Html, div)
import Html.Attributes exposing (href, style)
import Interpolation exposing (Interpolator)


type alias Model =
    { startColor : Color
    , endColor : Color
    , count : Int
    }


palette : Model -> (Color -> Color -> Interpolator Color) -> Html msg
palette model colorSpaceInterpolator =
    div [ style "display" "flex", style "flex-grow" "1" ]
        (Interpolation.samples model.count
            (colorSpaceInterpolator model.startColor model.endColor)
            |> List.map
                (\color ->
                    Html.div [ style "background-color" (Color.toCssString color), style "flex-grow" "1" ] []
                )
        )


view : Model -> Html msg
view model =
    Html.div [ style "display" "flex", style "min-height" "100vh", style "padding-right" "10px", style "flex-grow" "1" ]
        [ Html.div
            [ style "flex-grow" "1", style "margin-right" "20px", style "display" "flex", style "flex-direction" "column" ]
            [ title "rgb" "https://en.wikipedia.org/wiki/RGB_color_model"
            , palette model Interpolation.rgb
            , title "hsl" "https://en.wikipedia.org/wiki/HSL_and_HSV"
            , palette model Interpolation.hsl
            , title "hslLong" "https://en.wikipedia.org/wiki/HSL_and_HSV"
            , palette model Interpolation.hslLong
            , title "lab" "https://en.wikipedia.org/wiki/CIELAB_color_space"
            , palette model Interpolation.lab
            , title "hcl" "https://en.wikipedia.org/wiki/HCL_color_space"
            , palette model Interpolation.hcl
            , title "hclLong" "https://en.wikipedia.org/wiki/HCL_color_space"
            , palette model Interpolation.hclLong
            ]
        ]


title : String -> String -> Html msg
title label url =
    Html.h3 [ style "margin-bottom" "5px", style "margin-left" "5px", style "font" "16px -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif" ]
        [ Html.a [ href url ] [ Html.text label ]
        ]


main : Example.Program Model
main =
    Example.configuration
        { startColor = Color.rgb255 0 255 0
        , endColor = Color.rgb255 255 2 0
        , count = 50
        }
        [ Example.colorPicker "Start Color" .startColor (\v m -> { m | startColor = v })
        , Example.colorPicker "End Color" .endColor (\v m -> { m | endColor = v })
        , Example.intSlider "Number of Colors" { min = 3, max = 100 } .count (\v m -> { m | count = v })
        ]
        |> Example.withTitle "Color Space Interpolations"
        |> Example.application view

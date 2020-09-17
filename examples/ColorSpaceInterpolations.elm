module ColorSpaceInterpolations exposing (main)

{-| This module shows how to build some simple colour space palettes.

@category Reference

-}

import Color exposing (Color, rgb255)
import Example
import Hex
import Html exposing (Html, div)
import Html.Attributes exposing (class, for, href, id, selected, style, type_, value)
import Html.Events exposing (onInput)
import Interpolation exposing (Interpolator)
import Scale exposing (QuantizeScale)


type alias Model =
    { fromColorValue : Color
    , toColorValue : Color
    , count : Int
    }


configuration : Example.Configuration Model
configuration =
    Example.configuration
        { fromColorValue = Color.rgb255 0 255 0
        , toColorValue = Color.rgb255 255 2 0
        , count = 50
        }
        [ Example.colorPicker "Start Color" .fromColorValue (\v m -> { m | fromColorValue = v })
        , Example.colorPicker "End Color" .toColorValue (\v m -> { m | toColorValue = v })
        , Example.intSlider "Number of Colors" .count (\v m -> { m | count = v }) 3 100
        ]


palette : Model -> (Color -> Color -> Interpolator Color) -> Html msg
palette model colorSpaceInterpolator =
    div [ style "display" "flex", style "flex-grow" "1" ]
        (Interpolation.samples model.count
            (colorSpaceInterpolator model.fromColorValue model.toColorValue)
            |> List.map
                (\color ->
                    Html.div [ style "background-color" (Color.toCssString color), style "flex-grow" "1" ] []
                )
        )


view : Model -> Html (Example.ConfigMsg Model)
view model =
    Html.div [ style "display" "flex", style "min-height" "100vh", style "padding-right" "10px" ]
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
        , Example.verticalPanel "Color Space Interpolations" configuration model
        ]


title : String -> String -> Html msg
title label url =
    Html.h3 [ style "margin-bottom" "5px", style "margin-left" "5px", style "font" "16px -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif" ]
        [ Html.a [ href url ] [ Html.text label ]
        ]


main =
    Example.configurable configuration view

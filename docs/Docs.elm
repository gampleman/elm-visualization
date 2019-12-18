module Docs exposing (main)

import Css exposing (..)
import Css.Global
import Css.Media
import ExamplePublisher exposing (Document, Example)
import Html
import Html.Attributes
import Html.Styled exposing (a, div, h1, h2, h3, header, img, li, main_, nav, source, text, ul)
import Html.Styled.Attributes exposing (alt, css, href, rel, src, type_)
import Json.Decode


displayName : { a | basename : String } -> String
displayName example =
    example.basename
        |> String.toList
        |> List.concatMap
            (\char ->
                if Char.isUpper char then
                    [ ' ', char ]

                else
                    [ char ]
            )
        |> String.fromList


picture =
    Html.Styled.node "picture"


srcset =
    Html.Styled.Attributes.attribute "srcset"


main =
    ExamplePublisher.application
        { tagDecoder = Json.Decode.succeed ()
        , indexView = indexView
        , showView = showView
        }


onMobile =
    Css.Media.withMedia [ Css.Media.only Css.Media.screen [ Css.Media.maxWidth (px 900) ] ]


notVisibleOnMobile =
    onMobile [ display none ]


linkStyle =
    Css.batch
        [ color (hex "#1184CE")
        , textDecoration none
        , hover
            [ color (rgb 234 21 122)
            , textDecoration underline
            ]
        ]


headerView =
    div
        [ css
            [ borderBottom3 (px 1) solid (hex "#60B5CC")
            , displayFlex
            , alignItems center
            , justifyContent spaceBetween
            ]
        ]
        [ header
            [ css
                [ displayFlex
                , alignItems center
                ]
            ]
            [ h1
                [ css
                    [ fontWeight normal
                    , margin zero
                    , marginLeft (px 20)
                    , lineHeight zero
                    ]
                ]
                [ img [ css [ maxWidth (px 421), width (pct 100) ], src "assets/logo-inline.png", alt "ELM-VISUALIZATION" ] [] ]
            , h2
                [ css
                    [ margin zero
                    , marginLeft (px 20)
                    , fontSize (px 24)
                    , color (hex "#000")
                    , notVisibleOnMobile
                    , fontWeight normal
                    ]
                ]
                [ text "examples" ]
            ]
        , nav
            [ css
                [ displayFlex
                , alignItems center
                , notVisibleOnMobile
                ]
            ]
            [ a [ css [ marginRight (px 20), linkStyle ], href "https://package.elm-lang.org/packages/gampleman/elm-visualization/latest/" ] [ text "Docs" ]
            , a [ css [ marginRight (px 20), linkStyle ], href "https://github.com/gampleman/elm-visualization" ] [ text "GitHub" ]
            ]
        ]


mainView examples =
    main_ []
        [ ul
            [ css
                [ listStyleType none
                , padding zero
                ]
            ]
          <|
            List.map
                (\example ->
                    li
                        [ css
                            [ border3 (px 1) solid (hex "#eeeeee")
                            , float left
                            , width (px (toFloat (example.width // 3 + 20)))
                            , maxWidth (calc (vh 100) minus (px 40))
                            , margin (px 40)
                            , padding4 (px 10) (px 10) zero (px 10)
                            , boxSizing borderBox
                            , onMobile
                                [ maxWidth (pct 100)
                                , height auto
                                ]
                            ]
                        ]
                        [ a [ href example.basename, css [ linkStyle ] ] [ examplePreview example, h3 [ css [ marginLeft (px 10), fontWeight normal ] ] [ text (displayName example) ] ] ]
                )
                examples
        ]


makeSrcset example format =
    example.basename
        ++ "/preview."
        ++ format
        ++ " 1x, "
        ++ example.basename
        ++ "/preview@2x."
        ++ format
        ++ " 2x,"
        ++ example.basename
        ++ "/preview@3x."
        ++ format
        ++ " 3x"
        |> srcset


examplePreview example =
    picture []
        [ source [ makeSrcset example "webp", type_ "image/webp" ] []
        , source [ makeSrcset example "png", type_ "image/png" ] []
        , img [ src (example.basename ++ "/preview.png"), Html.Styled.Attributes.width (example.width // 3), Html.Styled.Attributes.height (example.height // 3), alt "" ] []
        ]


globalStyles =
    [ Html.Styled.node "link" [ href "https://fonts.googleapis.com/css?family=Source+Sans+Pro:400,700,400italic,700italic|Source+Code+Pro", rel "stylesheet" ] []
    , Css.Global.global
        [ Css.Global.each [ Css.Global.body, Css.Global.html ]
            [ fontFamilies [ qt "Source Sans Pro", qt "Trebuchet MS", qt "Lucida Grande", qt "Helvetica Neue", sansSerif.value ]
            , color (hex "#293c4b")
            , margin zero
            , height (pct 100)
            ]
        ]
    ]


indexView examples =
    { title = "Elm-Visualization Examples"
    , meta =
        [ ( "description", "Examples of using the Elm-Visualization library for making charts and data graphics." )
        , ( "viewport", "width=device-width, initial-scale=1.0" )
        ]
    , body =
        [ Html.Styled.toUnstyled <|
            div [] (headerView :: mainView examples :: globalStyles)
        ]
    }


showView example examples =
    { title = "Show" ++ example.basename
    , meta = []
    , body =
        [ Html.Styled.toUnstyled <| text ("Show" ++ example.basename)
        ]
    }

module Docs exposing (main)

import Css exposing (..)
import Css.Global
import Css.Media
import ExamplePublisher exposing (Document, Example)
import Html
import Html.Attributes
import Html.Styled exposing (Html, a, aside, code, div, h1, h2, h3, header, iframe, img, li, main_, nav, source, span, text, ul)
import Html.Styled.Attributes as A exposing (alt, class, css, href, rel, src, type_)
import Json.Decode
import Markdown


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
    A.attribute "srcset"


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


headerView : Maybe (Example tags) -> Html msg
headerView currentExample =
    div
        [ css
            [ borderBottom3 (px 1) solid (hex "#60B5CC")
            , displayFlex
            , alignItems center
            , justifyContent
                (case currentExample of
                    Just ex ->
                        center

                    Nothing ->
                        spaceBetween
                )
            , width (pct 100)
            ]
        ]
        ([ header
            [ css
                ([ displayFlex
                 , alignItems center
                 ]
                    ++ (case currentExample of
                            Just ex ->
                                [ maxWidth (px (toFloat ex.width + 316))
                                , width (pct 100)
                                ]

                            Nothing ->
                                []
                       )
                )
            ]
            ([ h1
                [ css
                    [ fontWeight normal
                    , margin zero
                    , marginLeft (px 20)
                    , lineHeight zero
                    ]
                ]
                [ img
                    [ css [ maxWidth (px 421), width (pct 100) ]
                    , src
                        (case currentExample of
                            Just _ ->
                                "../assets/logo-inline.png"

                            Nothing ->
                                "assets/logo-inline.png"
                        )
                    , alt "ELM-VISUALIZATION"
                    ]
                    []
                ]
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
                (case currentExample of
                    Just _ ->
                        [ a [ css [ linkStyle ], href "../" ] [ text "examples" ] ]

                    Nothing ->
                        [ text "examples" ]
                )
             ]
                ++ (case currentExample of
                        Just example ->
                            [ span [ css [ margin2 zero (px 10) ] ] [ text "/" ]
                            , h1
                                [ css
                                    [ notVisibleOnMobile
                                    , fontWeight normal
                                    , margin zero

                                    -- , marginLeft (px 20)
                                    , lineHeight zero
                                    , fontSize (px 24)
                                    ]
                                ]
                                [ text (displayName example) ]
                            ]

                        Nothing ->
                            []
                   )
            )
         ]
            ++ (case currentExample of
                    Just _ ->
                        []

                    Nothing ->
                        [ nav
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
               )
        )


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
        , img [ src (example.basename ++ "/preview.png"), A.width (example.width // 3), A.height (example.height // 3), alt "" ] []
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
            div [] (headerView Nothing :: mainView examples :: globalStyles)
        ]
    }


showView example examples =
    { title = "Show" ++ example.basename
    , meta = []
    , body =
        [ Html.Styled.toUnstyled <|
            div
                [ css
                    [ displayFlex
                    , flexDirection column
                    , alignItems center
                    , width (pct 100)
                    ]
                ]
                (headerView (Just example) :: exampleView example examples :: Html.Styled.node "link" [ href "../assets/syntax.css", rel "stylesheet" ] [] :: globalStyles)
        ]
    }


exampleView example examples =
    div
        [ css
            [ displayFlex
            , flexDirection row
            , width (pct 100)
            , maxWidth (px (toFloat example.width + 300))
            , marginTop (px 20)
            ]
        ]
        [ main_
            [ css
                [ width (calc (vw 100) minus (px 315))
                , maxWidth (px (toFloat example.width + 16))
                , paddingLeft (px 15)

                -- , flexShrink 1
                -- , flexGrow 1
                ]
            ]
            [ div
                [ css
                    [ position relative
                    , overflow hidden
                    , paddingTop (pct (toFloat example.height / toFloat example.width * 100))
                    , maxWidth (px (toFloat example.width + 16))
                    ]
                ]
                [ iframe
                    [ src "iframe.html"
                    , A.attribute "frameborder" "1"
                    , A.attribute "scrolling" "no"
                    , A.title (displayName example ++ " example")
                    , css
                        [ border3 (px 1) solid (hex "#eee")
                        , overflow hidden
                        , position absolute
                        , top zero
                        , left zero
                        , width (pct 100)
                        , height (pct 100)
                        , border zero
                        ]
                    ]
                    []
                ]
            , h2 [] [ text (displayName example) ]
            , div []
                [ markdown example.description
                ]
            , Html.Styled.pre [ css [ fontSize (px 14), overflow auto, property "wordWrap" "normal" ] ]
                [ code [ class "elm" ] [ text example.source ]
                ]
            ]
        , aside
            [ css
                [ width (px 300)
                , paddingLeft (px 15)
                , overflow hidden
                ]
            ]
            [ ul [ css [ padding zero ] ]
                [ li [ css [ asideListStyle ] ] [ a [ href "https://package.elm-lang.org/packages/gampleman/elm-visualization/latest/", css [ linkStyle ] ] [ text "Docs" ] ]
                , li [ css [ asideListStyle ] ] [ a [ href "https://github.com/gampleman/elm-visualization", css [ linkStyle ] ] [ text "GitHub" ] ]
                , li [ css [ asideListStyle ] ] [ a [ href "https://github.com/gampleman/elm-visualization/releases", css [ linkStyle ] ] [ text "Changelog" ] ]
                , li [ css [ asideListStyle ] ]
                    [ text "#visualization on "
                    , a [ href "https://elmlang.herokuapp.com/", css [ linkStyle ] ] [ text "Elm slack" ]
                    ]
                ]
            , h2 [] [ text "Examples" ]
            , examples
                |> List.map
                    (\ex ->
                        li [ css [ asideListStyle ] ]
                            [ a
                                [ href ("../" ++ ex.basename)
                                , css
                                    (linkStyle
                                        :: (if ex == example then
                                                [ textDecoration underline, fontWeight bold ]

                                            else
                                                []
                                           )
                                    )
                                ]
                                [ text (displayName ex) ]
                            ]
                    )
                |> ul [ css [ padding zero ] ]
            ]
        ]


asideListStyle =
    Css.batch
        [ listStyleType none
        , marginBottom (px 10)
        ]


markdown =
    Markdown.toHtmlWith
        { githubFlavored = Just { tables = True, breaks = False }
        , defaultHighlighting = Nothing
        , sanitize = False
        , smartypants = True
        }
        []
        >> Html.Styled.fromUnstyled

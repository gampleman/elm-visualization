module Docs exposing (main)

import Css exposing (..)
import Css.Global
import Css.Media
import Dict exposing (Dict)
import ExamplePublisher exposing (Document)
import Html
import Html.Attributes
import Html.Styled exposing (Attribute, Html, a, aside, code, div, h1, h2, h3, header, iframe, img, li, main_, nav, p, section, source, span, styled, text, ul)
import Html.Styled.Attributes as A exposing (alt, class, css, href, rel, src, type_)
import Json.Decode as Decode
import MyMarkdown


projectName : String
projectName =
    "elm-visualization"


authorName : String
authorName =
    "gampleman"

baseUrl : String 
baseUrl =
    "https://elm-visualization.netlify.app/"


type alias Example =
    ExamplePublisher.Example { category : String }


byCategory : List Example -> Dict String (List Example)
byCategory =
    List.foldl (\item -> Dict.update item.tags.category (Maybe.map ((::) item) >> Maybe.withDefault [ item ] >> Just)) Dict.empty
        >> Dict.map (\_ l -> List.reverse l)


categories : List ( String, String )
categories =
    [ ( "Basics", "Simple examples to get you started making simple charts and graphs." )
    , ( "Advanced", "More complex charts or illustrations of trickier techniques." )
    , ( "Art", "You can use elm-visualization to make some pretty cool art." )
    , ( "Reference", "Examples here aren't meant as standalone, but rather illustrate some details of how the different features of elm-visualization works." )
    ]


{-| ExamplePublisher.application is the entry point we are given for this templating system. It has 3 fields:

  - `tagDecoder` is a JSON decoder for any `@tags` that the examples have. This mechanism allows us to augment the `Example` type by arbitrary data. This allows us the implement any features we like.
  - `indexView` is a view function that recieves all the examples and must return a `Document`, which has a `body`, `title` and finally `meta` which allows us to generate meta tags.
  - `showView` takes the current example and a list of all examples. The tool will invoke this function for each example.

-}
main : ExamplePublisher.Program { category : String }
main =
    ExamplePublisher.application
        { tagDecoder = Decode.map (\category -> { category = category }) (Decode.field "category" Decode.string)
        , indexView = indexView
        , showView = showView
        }


indexView : List Example -> Document
indexView examples =
    { title = projectName ++ " Examples"
    , meta =
        -- a better description would be a good idea!
        [ ( "description", "Examples of using elm-visualization to make charts and data graphics" )
        , ( "viewport", "width=device-width, initial-scale=1.0" )
        ]
    , body =
        [ Html.Styled.toUnstyled <|
            div [] (headerView Nothing :: mainView examples :: globalStyles)
        ]
    }


showView : Example -> List Example -> Document
showView example examples =
    { title = projectName ++ " " ++ example.basename ++ " Example"
    , meta = [ ( "viewport", "width=device-width, initial-scale=1.0" )
    , ("twitter:card", "summary_large_image")
    , ("twitter:description", String.left 200 (MyMarkdown.strip example.description))
    , ("og:description", String.left 200 (MyMarkdown.strip example.description))
    , ("twitter:title", displayName example ++ " " ++ projectName ++ " example"  ) 
    , ("og:title", displayName example ++ " – " ++ projectName  )
    , ("twitter:image", baseUrl ++ example.basename ++ "/preview@3x.png")
    , ("og:image", baseUrl ++ example.basename ++ "/preview@3x.png")
     , ("twitter:image:alt", "A rendering of the chart")
    , ("og:image:akt", "A rendering of the chart")

    ]
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



-- Helpers


{-| This will humanize the Example name to be more human readable.
-}
displayName : { a | basename : String } -> String
displayName =
    .basename >> humanize


humanize : String -> String
humanize =
    String.toList
        >> List.concatMap
            (\char ->
                if Char.isUpper char then
                    [ ' ', char ]

                else
                    [ char ]
            )
        >> String.fromList
        >> String.split " "
        >> List.map
            (\word ->
                if word == "And" then
                    "and"

                else if word == "With" then
                    "with"

                else
                    word
            )
        >> String.join " "


{-| Formats a bit of markdown.
-}
markdown : String -> Html a
markdown =
    MyMarkdown.render
        >> Html.Styled.fromUnstyled


picture : List (Attribute msg) -> List (Html msg) -> Html msg
picture =
    Html.Styled.node "picture"


onMobile : List Style -> Style
onMobile =
    Css.Media.withMedia [ Css.Media.only Css.Media.screen [ Css.Media.maxWidth (px 900) ] ]


notVisibleOnMobile : Style
notVisibleOnMobile =
    onMobile [ display none ]


{-| The build process has a little special case for this and will automatically run highlight.js to syntax highlight this code.
-}
highlightedSourceCode : String -> Html a
highlightedSourceCode source =
    Html.Styled.pre [ css [ fontSize (px 14), overflow auto, property "wordWrap" "normal" ] ]
        [ code [ class "elm" ] [ text source ]
        ]


linkStyle : Style
linkStyle =
    Css.batch
        [ color (hex "#1184CE")
        , textDecoration none
        , hover
            [ color (rgb 234 21 122)
            , textDecoration underline
            ]
        ]


{-| This will actually embed the rendered example as an iFrame, but making sure it can shrink in an aspect preserving way for smaller devices.
-}
responsiveExampleFrame : Example -> Html a
responsiveExampleFrame example =
    div
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
                [ overflow hidden
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


{-| Builds a responsive srcset for an example. Format should be either "png" or "webp".
-}
srcset : Example -> String -> Attribute msg
srcset example format =
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
        |> A.attribute "srcset"


{-| This will show a retina ready version of the thumbnail of the example. Images have gotten pretty complicated these days.
-}
examplePreview : Example -> Html msg
examplePreview example =
    picture []
        [ source [ srcset example "webp", type_ "image/webp" ] []
        , source [ srcset example "png", type_ "image/png" ] []
        , img [ src (example.basename ++ "/preview.png"), A.width (example.width // 3), A.height (example.height // 3), alt "" ] []
        ]


{-| This is the main header. You will probably want to customize it for your project.
-}
headerView : Maybe Example -> Html msg
headerView currentExample =
    div
        [ css
            [ displayFlex
            , backgroundColor (hex "#5FABDC")
            , color (hex "#fff")
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
                 , padding2 (px 10) (px 0)
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
                -- You may want to replace this with your project logo are at least your project name
                [ css
                    [ fontWeight normal
                    , margin zero
                    , marginLeft (px 20)
                    , lineHeight zero
                    ]
                ]
                [ a
                    [ Maybe.map (always "../") currentExample |> Maybe.withDefault "." |> href
                    , css [ displayFlex, color (hex "#fff"), textDecoration none ]
                    ]
                    [ img
                        [ src
                            (case currentExample of
                                Just _ ->
                                    "../assets/elm-logo.svg"

                                Nothing ->
                                    "assets/elm-logo.svg"
                            )
                        , alt "Elm logo"
                        ]
                        []
                    , div [ css [ paddingLeft (px 8) ] ]
                        [ div
                            [ css
                                [ lineHeight (px 20)
                                , fontSize (px 30)
                                , transform (translateY (px -4))
                                ]
                            ]
                            [ text "elm" ]
                        , div
                            [ css
                                [ lineHeight (px 10)
                                , fontSize (px 12)
                                , fontWeight (int 600)
                                ]
                            ]
                            [ text "examples" ]
                        ]
                    ]
                ]
             , h2
                [ css
                    [ margin zero
                    , marginLeft (px 20)
                    , fontSize (px 24)
                    , notVisibleOnMobile
                    , fontWeight normal
                    ]
                ]
                [ text projectName ]
             , span [ css [ margin2 zero (px 10), notVisibleOnMobile ] ] [ text "/" ]
             , h2
                [ css
                    [ margin zero
                    , fontSize (px 24)
                    , notVisibleOnMobile
                    , fontWeight normal
                    ]
                ]
                (case currentExample of
                    Just _ ->
                        [ a [ css [ linkStyle, color (hex "#fff") ], href "../" ] [ text "examples" ] ]

                    Nothing ->
                        [ text "examples" ]
                )
             ]
                ++ (case currentExample of
                        Just example ->
                            [ span [ css [ margin4 zero zero zero (px 10), notVisibleOnMobile ] ] [ text "/" ]
                            , h1
                                [ css
                                    [ fontWeight normal
                                    , margin zero
                                    , marginLeft (px 10)
                                    , lineHeight zero
                                    , fontSize (px 24)
                                    , textOverflow ellipsis
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
                            [ -- You may want to include some project links here:
                              a [ css [ marginRight (px 20), linkStyle, color (hex "#fff")], href ("https://package.elm-lang.org/packages/" ++ authorName ++ "/" ++ projectName ++ "/latest/") ] [ text "Docs" ]
                            , a [ css [ marginRight (px 20), linkStyle, color (hex "#fff") ], href ("https://github.com/" ++ authorName ++ "/" ++ projectName) ] [ text "GitHub" ]
                            ]
                        ]
               )
        )


mainView : List Example -> Html a
mainView examples =
    let
        categorizedExample =
            byCategory examples
    in
    main_ [ css [ paddingBottom (px 80) ] ] <|
        List.map
            (\( category, description ) ->
                section []
                    [ h3 [ css [ marginLeft (px 20), marginTop (px 35), property "clear" "both" ] ] [ text category ]
                    , p [ css [ marginLeft (px 20) ] ] [ text description ]
                    , categorizedExample
                        |> Dict.get category
                        |> Maybe.withDefault []
                        |> List.map
                            (\example ->
                                li
                                    [ css
                                        [ border3 (px 1) solid (hex "#eeeeee")
                                        , float left
                                        , width (px (toFloat (example.width // 3 + 20)))
                                        , maxWidth (calc (vh 100) minus (px 40))
                                        , margin (px 20)
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
                        |> ul
                            [ css
                                [ listStyleType none
                                , padding zero
                                ]
                            ]
                    , div [ css [  property "clear" "both" ]] []
                    ]
            )
            categories


{-| This is here mostly to set up basic typography.
-}
globalStyles : List (Html a)
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


navItem : String -> String -> Bool -> Html a
navItem url label active =
    li
        [ css
            [ listStyleType none
            , lineHeight (num 1.5)
            ]
        ]
        [ a
            [ href url
            , css
                (linkStyle
                    :: (if active then
                            [ textDecoration underline, fontWeight bold ]

                        else
                            []
                       )
                )
            ]
            [ text label ]
        ]


ellieLinkView : Example -> Html msg
ellieLinkView example =
    case example.ellieLink of
        Just url ->
            a [ href url, css [ linkStyle, float right, margin (px 28) ] ] [ text "Edit on Ellie" ]

        Nothing ->
            text ""


exampleView : Example -> List Example -> Html msg
exampleView example examples =
    let
        fromCategory =
            byCategory examples
                |> Dict.get example.tags.category
                |> Maybe.withDefault []
    in
    div
        [ css
            [ displayFlex
            , flexDirection row
            , alignItems flexStart
            , width (pct 100)
            , maxWidth (px (toFloat example.width + 300))
            , marginTop (px 20)
            ]
        ]
        [ main_
            [ css
                [ width (calc (vw 100) minus (px 315))
                , onMobile [ width (pct 100) ]
                , maxWidth (px (toFloat example.width + 16))
                , paddingLeft (px 15)
                , boxSizing borderBox
                ]
            ]
            [ responsiveExampleFrame example
            , ellieLinkView example
            , h2 [ css [ fontWeight normal ] ] [ text (displayName example) ]
            , markdown example.description
            , highlightedSourceCode example.source
            ]
        , aside
            [ css
                [ notVisibleOnMobile
                , width (px 300)
                , paddingLeft (px 30)
                , overflowX hidden
                , borderLeft3 (px 1) solid (hex "#eee")
                , position sticky
                , top (px 0)
                , maxHeight (vh 100)
                , overflowY auto
                ]
            ]
            [ -- You may want to put some project links here
              ul [ css [ padding zero ] ]
                [ navItem ("https://package.elm-lang.org/packages/" ++ authorName ++ "/" ++ projectName ++ "/latest/") "Docs" False
                , navItem ("https://github.com/" ++ authorName ++ "/" ++ projectName) "GitHub" False
                , navItem ("https://github.com/" ++ authorName ++ "/" ++ projectName ++ "/releases") "Changelog" False
                , navItem "https://elmlang.slack.com/channels/visualization" "#visualization on Elm slack" False
                ]
            , h2 [ css [ fontWeight normal, marginBottom (px 10) ] ] [ text example.tags.category ]
            , fromCategory
                |> List.map (\ex -> navItem ("../" ++ ex.basename) (displayName ex) (ex == example))
                |> ul [ css [ padding zero ] ]
            ]
        ]

module Example exposing (error, linkTo, loading, navigation, switchableViews)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Navigation exposing (Key)
import Html exposing (Html, div, text, th)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import Http
import Url exposing (Url)



-- Common Views


loading : List (Html.Attribute msg) -> Html msg
loading attrs =
    div (style "display" "flex" :: style "flex-direction" "column" :: style "align-items" "center" :: style "max-width" "900px" :: style "margin-top" "50px" :: attrs)
        [ Html.node "style" [] [ text """
        .lds-ring {
            display: inline-block;
            position: relative;
            width: 60px;
            height: 60px;
        }
        .lds-ring div {
            box-sizing: border-box;
            display: block;
            position: absolute;
            width: 44px;
            height: 44px;
            margin: 8px;
            border: 4px solid #fff;
            border-radius: 50%;
            animation: lds-ring 1.2s cubic-bezier(0.5, 0, 0.5, 1) infinite;
            border-color: #000 transparent transparent transparent;
        }
        .lds-ring div:nth-child(1) {
            animation-delay: -0.45s;
        }
            .lds-ring div:nth-child(2) {
            animation-delay: -0.3s;
        }
        .lds-ring div:nth-child(3) {
            animation-delay: -0.15s;
        }
        @keyframes lds-ring {
            0% {
                transform: rotate(0deg);
            }
            100% {
                transform: rotate(360deg);
            }
        }
""" ]
        , div [ class "lds-ring" ] [ div [] [], div [] [], div [] [] ]
        , div [ class "lds-label" ] [ text "Loading..." ]
        ]


error : Maybe msg -> Http.Error -> Html msg
error maybeTryAgain err =
    let
        wrapInRetry el =
            case maybeTryAgain of
                Just tryAgain ->
                    div []
                        [ el
                        , Html.button [ onClick tryAgain ] [ text "Try again?" ]
                        ]

                Nothing ->
                    el
    in
    case err of
        Http.BadUrl url ->
            text <| "The url requested (" ++ url ++ ") was not valid."

        Http.Timeout ->
            wrapInRetry (text "The requested timed out.")

        Http.NetworkError ->
            wrapInRetry (text "You appear to be offline.")

        Http.BadStatus status ->
            case status of
                400 ->
                    div []
                        [ Html.h4 [] [ text "400 Bad Request" ]
                        , text "The request was malformed and the server couldn't process it."
                        ]

                403 ->
                    div []
                        [ Html.h4 [] [ text "403 Forbidden" ]
                        , text "You are not authorized to access this resource."
                        ]

                404 ->
                    wrapInRetry <|
                        div []
                            [ Html.h4 [] [ text "404 Not Found" ]
                            , text "The requested resource could not be found."
                            ]

                500 ->
                    wrapInRetry <|
                        div []
                            [ Html.h4 [] [ text "500 Internal Server Error" ]
                            , text "Something went unexpectedly wrong in the server."
                            ]

                otherwise ->
                    text <| "Your request failed with status code: " ++ String.fromInt status

        Http.BadBody errorStr ->
            Html.details []
                [ Html.summary [] [ text "Parsing of the request body failed." ]
                , Html.pre [ Html.Attributes.style "white-space" "pre-wrap" ]
                    [ text errorStr
                    ]
                ]



-- Mutli-tab apps


stringify : String -> String
stringify =
    String.toLower
        >> String.map
            (\char ->
                case char of
                    ' ' ->
                        '-'

                    ':' ->
                        '-'

                    a ->
                        a
            )


parse : Config a -> Url -> Maybe String
parse conf url =
    url.fragment


view : List ( String, a ) -> (a -> Html msg) -> Model -> Document Msg
view config viewFn model =
    let
        ( route, subView ) =
            if model.view == "" then
                List.head config
                    |> Maybe.map (\( r, m ) -> ( r, viewFn m ))
                    |> Maybe.withDefault ( "Not found", Html.text "404 not found" )

            else
                List.filter (\( l, _ ) -> stringify l == model.view) config
                    |> List.head
                    |> Maybe.map (\( r, m ) -> ( r, viewFn m ))
                    |> Maybe.withDefault ( "Not found", Html.text "404 not found" )
    in
    { body = [ Html.map (always Noop) subView ], title = route }


navigation : String -> List ( String, a ) -> Html msg
navigation title config =
    Html.p [] <|
        text
            (title ++ ": ")
            :: (List.intersperse (text " | ") <| List.map (\( a, _ ) -> linkTo a [] [ text a ]) config)


switchableViews : Config a -> (a -> Html msg) -> Program () Model Msg
switchableViews config viewFn =
    Browser.application
        { init = init config
        , update = update
        , view = view config viewFn
        , subscriptions = always Sub.none
        , onUrlRequest = onUrlRequest config
        , onUrlChange = onUrlChange config
        }


type alias Config a =
    List ( String, a )


type Msg
    = Noop
    | View String
    | ExternalUrl String


type alias Model =
    { view : String
    , key : Key
    }


init : Config a -> () -> Url -> Key -> ( Model, Cmd Msg )
init config () url key =
    case parse config url of
        Just viewPath ->
            ( { view = viewPath, key = key }, Cmd.none )

        Nothing ->
            let
                defaultPath =
                    List.head config
                        |> Maybe.map Tuple.first
                        |> Maybe.withDefault ""
                        |> stringify
            in
            ( { view = "", key = key }, Navigation.replaceUrl key ("#" ++ defaultPath) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        View viewPath ->
            ( { model | view = viewPath }
            , if model.view == viewPath then
                Cmd.none

              else
                Navigation.pushUrl model.key ("#" ++ viewPath)
            )

        ExternalUrl url ->
            ( model, Navigation.load url )


onUrlRequest : Config a -> UrlRequest -> Msg
onUrlRequest config urlRequest =
    case urlRequest of
        Internal url ->
            View (parse config url |> Maybe.withDefault "")

        External url ->
            ExternalUrl url


onUrlChange : Config a -> Url -> Msg
onUrlChange config url =
    View (parse config url |> Maybe.withDefault "")


linkTo : String -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
linkTo viewId attrs children =
    let
        hrefAttr =
            href ("#" ++ stringify viewId)

        attributes =
            hrefAttr :: attrs
    in
    Html.a attributes children

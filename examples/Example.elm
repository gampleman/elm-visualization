module Example exposing (linkTo, navigation, switchableViews)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Navigation exposing (Key)
import Html exposing (Html, text, th)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Url exposing (Url)


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

module Example exposing (switchableViews, linkTo, navigation)

import Html exposing (Html, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Navigation exposing (Location)


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


toMsg : { b | hash : String } -> String
toMsg { hash } =
    case String.uncons hash of
        Just ( '#', rest ) ->
            rest

        otherwise ->
            ""


view : List ( String, a ) -> (a -> Html String) -> String -> Html String
view config viewFn location =
    List.filter (\( l, _ ) -> stringify l == location) config
        |> List.head
        |> Maybe.map Tuple.second
        |> Maybe.map viewFn
        |> Maybe.withDefault (List.head config |> Maybe.map Tuple.second |> Maybe.map viewFn |> Maybe.withDefault (Html.text "404 not found"))


navigation : String -> List ( String, a ) -> Html String
navigation title config =
    Html.p [] <|
        text
            (title ++ ": ")
            :: (List.intersperse (text " | ") <| List.map (\( a, _ ) -> linkTo a [] [ text a ]) config)


switchableViews : List ( String, a ) -> (a -> Html String) -> Program Never String String
switchableViews config viewFn =
    Navigation.program toMsg
        { init = \l -> ( toMsg l, Cmd.none )
        , update = \msg oldMsg -> ( msg, Cmd.none )
        , view = view (Debug.log "config" config) viewFn
        , subscriptions = always Sub.none
        }


linkTo : String -> List (Html.Attribute String) -> List (Html String) -> Html String
linkTo view attrs children =
    let
        clickHandler =
            onClick (stringify view)

        hrefAttr =
            href ("#" ++ stringify view)

        attributes =
            clickHandler :: hrefAttr :: attrs
    in
        Html.a attributes children

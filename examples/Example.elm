module Example exposing (switchableViews, linkTo)

import Html exposing (Html)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Navigation exposing (Location)


stringify : a -> String
stringify =
    toString >> String.toLower


toMsg : { b | hash : String } -> String
toMsg { hash } =
    case String.uncons hash of
        Just ( '#', rest ) ->
            rest

        otherwise ->
            ""


view : List ( a, Html String ) -> String -> Html String
view config location =
    List.filter (\( l, _ ) -> stringify l == location) config
        |> List.head
        |> Maybe.map Tuple.second
        |> Maybe.withDefault (List.head config |> Maybe.map Tuple.second |> Maybe.withDefault (Html.text "404 not found"))


switchableViews : List ( a, Html String ) -> Program Never String String
switchableViews config =
    Navigation.program toMsg
        { init = \l -> ( toMsg l, Cmd.none )
        , update = \msg oldMsg -> ( msg, Cmd.none )
        , view = view config
        , subscriptions = always Sub.none
        }


linkTo : a -> List (Html.Attribute String) -> List (Html String) -> Html String
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

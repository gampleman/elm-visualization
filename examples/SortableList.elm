module SortableList exposing (main)

import Browser
import Browser.Events
import Html exposing (Html, button, div, input, label, li, text, ul)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Interpolation exposing (Interpolator)
import Time
import Transition exposing (Transition)


type alias Model =
    { list : List Record
    , nextId : Int
    , textEntered : String
    , transition : Transition (List Record)
    }


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


initialList =
    [ "Foo", "Bar", "Zyx", "Area", "Nook" ]
        |> List.indexedMap (\idx n -> { id = idx, title = n, position = toFloat idx * 20, opacity = 1 })


init : () -> ( Model, Cmd Msg )
init () =
    ( { list = initialList, nextId = 5, textEntered = "", transition = Transition.for 0 (always initialList) }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t ->
            ( { model | transition = Transition.play t model.transition }, Cmd.none )

        Typed str ->
            ( { model | textEntered = str }, Cmd.none )

        Add ->
            let
                newList =
                    { id = model.nextId, title = model.textEntered, position = 0, opacity = 1 } :: model.list |> recomputePositions
            in
            ( { model | list = newList, textEntered = "", nextId = model.nextId + 1 }
                |> updateAnimation model
            , Cmd.none
            )

        Delete id ->
            ( { model
                | list =
                    model.list
                        |> List.filter (\rec -> rec.id /= id)
                        |> recomputePositions
              }
                |> updateAnimation model
            , Cmd.none
            )

        Sort ->
            ( { model
                | list =
                    model.list
                        -- |> List.sortBy (\rec -> rec.title)
                        |> List.reverse
                        |> recomputePositions
              }
                |> updateAnimation model
            , Cmd.none
            )


updateAnimation oldModel newModel =
    { newModel | transition = Transition.for 1000 (interpolateList (Transition.value oldModel.transition) newModel.list) }


recomputePositions : List Record -> List Record
recomputePositions =
    List.indexedMap (\idx rec -> { rec | position = toFloat idx * 20 })


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrameDelta (round >> Tick)


type alias Record =
    { id : Int
    , title : String
    , position : Float
    , opacity : Float
    }


type Msg
    = Tick Int
    | Typed String
    | Add
    | Delete Int
    | Sort


view model =
    div [ style "display" "flex" ]
        [ ul [ style "position" "relative", style "width" "300px" ] <|
            List.map viewRec (Transition.value model.transition)
        , div []
            [ label []
                [ text "Title:"
                , input
                    [ type_ "text", onInput Typed, value model.textEntered ]
                    []
                ]
            , button [ onClick Add ] [ text "Add Item" ]
            , button [ onClick Sort ] [ text "Sort" ]
            ]
        ]


viewRec : Record -> Html Msg
viewRec record =
    li
        [ style "position" "absolute"
        , style "top" "0"
        , style "transform" ("translate(0, " ++ String.fromFloat record.position ++ "px)")
        , style "opacity" (String.fromFloat record.opacity)
        ]
        [ text record.title
        , button [ onClick (Delete record.id) ] [ text "x" ]
        ]


interpolateOpacity to rec =
    Interpolation.map (\op -> { rec | opacity = op })
        (Interpolation.float rec.opacity to)


interpolateList : List Record -> List Record -> Interpolator (List Record)
interpolateList =
    Interpolation.list
        { add = \rec -> interpolateOpacity 1 { rec | opacity = 0 }
        , remove = interpolateOpacity 0
        , change =
            \from to ->
                Interpolation.map2 (\op pos -> { from | opacity = op, position = pos })
                    (Interpolation.float from.opacity to.opacity)
                    (Interpolation.float from.position to.position)
        , id = .id
        , combine = Interpolation.combineParallel
        }

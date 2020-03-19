module TodoAnimated exposing (main)

{-| This is a modified version of [TodoMVC](http://todomvc.com) with the [original implementation here](https://github.com/tastejs/todomvc/blob/gh-pages/examples/elm/Todo.elm). This version adds animated transitions using elm-visualizations animation system.

@requires assets/todo.css
@delay 6

-}

import Browser
import Browser.Dom as Dom
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Interpolation exposing (Interpolator)
import Json.Decode as Json
import Task
import Transition exposing (Transition)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = updateWithTransition
        , subscriptions = subscriptions
        }



-- MODEL
-- The full application state of our todo app.


type alias Model =
    { entries : List Entry
    , field : String
    , uid : Int
    , visibility : String
    , transition : Transition (List Entry)
    }


type alias Entry =
    { description : String
    , completed : Bool
    , editing : Bool
    , id : Int
    , opacity : Float
    , position : Float
    }


emptyModel : Model
emptyModel =
    { entries = []
    , visibility = "All"
    , field = ""
    , uid = 0
    , transition = Transition.constant []
    }


newEntry : String -> Int -> Entry
newEntry desc id =
    { description = desc
    , completed = False
    , editing = False
    , id = id
    , opacity = 1
    , position = 0
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( emptyModel
    , Cmd.none
    )



-- UPDATE


{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
type Msg
    = NoOp
    | UpdateField String
    | EditingEntry Int Bool
    | UpdateEntry Int String
    | Add
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility String
    | Tick Int



-- How we update our Model on a given Msg?


updateWithTransition : Msg -> Model -> ( Model, Cmd Msg )
updateWithTransition msg oldModel =
    let
        ( newModel, cmd ) =
            update msg oldModel
    in
    ( updateTransition oldModel newModel, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Add ->
            let
                newEntries =
                    if String.isEmpty model.field then
                        model.entries

                    else
                        newEntry model.field model.uid :: model.entries
            in
            ( { model
                | uid = model.uid + 1
                , field = ""
                , entries = newEntries
              }
            , Cmd.none
            )

        UpdateField str ->
            ( { model | field = str }
            , Cmd.none
            )

        EditingEntry id isEditing ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | editing = isEditing }

                    else
                        t

                focus =
                    Dom.focus ("todo-" ++ String.fromInt id)
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Task.attempt (\_ -> NoOp) focus
            )

        UpdateEntry id task ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | description = task }

                    else
                        t
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Cmd.none
            )

        Delete id ->
            ( { model | entries = List.filter (\t -> t.id /= id) model.entries }
            , Cmd.none
            )

        DeleteComplete ->
            ( { model | entries = List.filter (not << .completed) model.entries }
            , Cmd.none
            )

        Check id isCompleted ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | completed = isCompleted }

                    else
                        t
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Cmd.none
            )

        CheckAll isCompleted ->
            let
                updateEntry t =
                    { t | completed = isCompleted }
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Cmd.none
            )

        ChangeVisibility visibility ->
            ( { model | visibility = visibility }
            , Cmd.none
            )

        Tick t ->
            ( { model | transition = Transition.step t model.transition }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if Transition.isComplete model.transition then
        Sub.none

    else
        Browser.Events.onAnimationFrameDelta (round >> Tick)



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "todomvc-wrapper"
        ]
        [ section
            [ class "todoapp" ]
            [ lazy viewInput model.field
            , viewEntries (Transition.value model.transition)
            , lazy2 viewControls model.visibility model.entries
            ]
        , infoFooter
        , node "link" [ attribute "href" "assets/todo.css", attribute "rel" "stylesheet" ] []
        ]


viewInput : String -> Html Msg
viewInput task =
    header
        [ class "header" ]
        [ h1 [] [ text "todos" ]
        , input
            [ class "new-todo"
            , placeholder "What needs to be done?"
            , autofocus True
            , value task
            , name "newTodo"
            , onInput UpdateField
            , onEnter Add
            ]
            []
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)



-- VIEW ALL ENTRIES


viewEntries : List Entry -> Html Msg
viewEntries entries =
    let
        allCompleted =
            List.all .completed entries

        cssVisibility =
            if List.isEmpty entries then
                "hidden"

            else
                "visible"
    in
    section
        [ class "main"
        , style "visibility" cssVisibility
        ]
        [ input
            [ id "toggle-all"
            , class "toggle-all"
            , type_ "checkbox"
            , name "toggle"
            , checked allCompleted
            , onClick (CheckAll (not allCompleted))
            ]
            []
        , label
            [ for "toggle-all" ]
            [ text "Mark all as complete" ]
        , Keyed.ul
            [ class "todo-list"
            , style "position" "relative"
            , style "height" (String.fromFloat (toFloat (List.length entries) * 58) ++ "px")
            , style "transition" "height 200ms"
            ]
          <|
            List.map viewKeyedEntry entries
        ]



-- VIEW INDIVIDUAL ENTRIES


viewKeyedEntry : Entry -> ( String, Html Msg )
viewKeyedEntry todo =
    ( String.fromInt todo.id, viewEntry todo )


viewEntry : Entry -> Html Msg
viewEntry todo =
    li
        [ classList [ ( "completed", todo.completed ), ( "editing", todo.editing ) ]
        , style "position" "absolute"
        , style "top" "0"
        , style "left" "0"
        , style "right" "0"
        , style "transform" ("translate(0, " ++ String.fromFloat (todo.position * 58) ++ "px)")
        , style "opacity" (String.fromFloat todo.opacity)
        ]
        [ div
            [ class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , checked todo.completed
                , onClick (Check todo.id (not todo.completed))
                ]
                []
            , label
                [ onDoubleClick (EditingEntry todo.id True) ]
                [ text todo.description ]
            , button
                [ class "destroy"
                , onClick (Delete todo.id)
                ]
                []
            ]
        , input
            [ class "edit"
            , value todo.description
            , name "title"
            , id ("todo-" ++ String.fromInt todo.id)
            , onInput (UpdateEntry todo.id)
            , onBlur (EditingEntry todo.id False)
            , onEnter (EditingEntry todo.id False)
            ]
            []
        ]



-- VIEW CONTROLS AND FOOTER


viewControls : String -> List Entry -> Html Msg
viewControls visibility entries =
    let
        entriesCompleted =
            List.length (List.filter .completed entries)

        entriesLeft =
            List.length entries - entriesCompleted
    in
    footer
        [ class "footer"
        , hidden (List.isEmpty entries)
        ]
        [ lazy viewControlsCount entriesLeft
        , lazy viewControlsFilters visibility
        , lazy viewControlsClear entriesCompleted
        ]


viewControlsCount : Int -> Html Msg
viewControlsCount entriesLeft =
    let
        item_ =
            if entriesLeft == 1 then
                " item"

            else
                " items"
    in
    span
        [ class "todo-count" ]
        [ strong [] [ text (String.fromInt entriesLeft) ]
        , text (item_ ++ " left")
        ]


viewControlsFilters : String -> Html Msg
viewControlsFilters visibility =
    ul
        [ class "filters" ]
        [ visibilitySwap "#/" "All" visibility
        , text " "
        , visibilitySwap "#/active" "Active" visibility
        , text " "
        , visibilitySwap "#/completed" "Completed" visibility
        ]


visibilitySwap : String -> String -> String -> Html Msg
visibilitySwap uri visibility actualVisibility =
    li
        [ onClick (ChangeVisibility visibility) ]
        [ a [ href uri, classList [ ( "selected", visibility == actualVisibility ) ] ]
            [ text visibility ]
        ]


viewControlsClear : Int -> Html Msg
viewControlsClear entriesCompleted =
    button
        [ class "clear-completed"
        , hidden (entriesCompleted == 0)
        , onClick DeleteComplete
        ]
        [ text ("Clear completed (" ++ String.fromInt entriesCompleted ++ ")")
        ]


infoFooter : Html msg
infoFooter =
    footer [ class "info" ]
        [ p [] [ text "Double-click to edit a todo" ]
        , p []
            [ text "Written by "
            , a [ href "https://github.com/evancz" ] [ text "Evan Czaplicki" ]
            , text " and modified by "
            , a [ href "https://github.com/gampleman" ] [ text "Jakub Hampl" ]
            ]
        , p []
            [ text "Part of "
            , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
            ]
        ]



-- ANIMATION


updateTransition : Model -> Model -> Model
updateTransition old new =
    let
        isVisible todo =
            case new.visibility of
                "Completed" ->
                    todo.completed

                "Active" ->
                    not todo.completed

                _ ->
                    True

        setPosition index todo =
            { todo | position = toFloat index }
    in
    if old.entries /= new.entries || old.visibility /= new.visibility then
        { new
            | transition =
                interpolateEntries
                    (Transition.value old.transition)
                    (List.filter isVisible new.entries |> List.indexedMap setPosition)
                    |> Transition.for 400
        }

    else
        new


interpolateEntries : List Entry -> List Entry -> Interpolator (List Entry)
interpolateEntries =
    Interpolation.list
        { add = \rec -> interpolateOpacity 1 { rec | opacity = 0 }
        , remove = interpolateOpacity 0
        , change =
            \from to ->
                Interpolation.map2 (\op pos -> { to | opacity = op, position = pos })
                    (Interpolation.float from.opacity to.opacity)
                    (Interpolation.float from.position to.position)
        , id = .id
        , combine = Interpolation.combineParallel
        }


interpolateOpacity : Float -> Entry -> Interpolator Entry
interpolateOpacity to rec =
    Interpolation.map (\op -> { rec | opacity = op })
        (Interpolation.float rec.opacity to)

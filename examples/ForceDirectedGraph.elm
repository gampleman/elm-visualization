module ForceDirectedGraph exposing (main)

import AnimationFrame
import Dict exposing (Dict)
import Graph exposing (Edge, Graph, Node, NodeId)
import Html
import Html.Events exposing (on)
import Json.Decode as Decode
import Miserables exposing (miserablesGraph)
import Mouse exposing (Position)
import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)
import Time exposing (Time)
import Visualization.Force as Force


screenWidth : Float
screenWidth =
    990


screenHeight : Float
screenHeight =
    450


type Msg
    = DragStart NodeId Position
    | DragAt Position
    | DragEnd Position
    | Tick Time


type alias Model =
    { drag : Maybe Drag
    , graph : Graph (Force.Entity String) ()
    , simulation : Force.State NodeId
    }


type alias Drag =
    { start : Position
    , current : Position
    , index : NodeId
    }


init : ( Model, Cmd Msg )
init =
    let
        graph =
            Graph.mapContexts
                (\({ node } as ctx) ->
                    { ctx | node = { label = Force.entity node.id node.label, id = node.id } }
                )
                miserablesGraph

        dict =
            graphToDict graph

        links =
            graph
                |> Graph.edges
                |> List.map (\{ from, to } -> { source = from, target = to })

        forces =
            [ Force.links links
            , Force.manyBody dict
            , Force.center (screenWidth / 2) (screenHeight / 2)
            ]
    in
        ( Model Nothing graph (Force.simulation forces), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateHelp msg model, Cmd.none )


updateNode pos nodeCtx =
    let
        nodeValue =
            nodeCtx.node.label

        node =
            nodeCtx.node
    in
        { nodeCtx | node = { node | label = { nodeValue | x = toFloat pos.x, y = toFloat pos.y } } }


updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
        { nodeCtx | node = { node | label = value } }


graphToDict : Graph (Force.Entity String) () -> Dict NodeId (Force.Entity String)
graphToDict =
    Graph.fold (\{ node } d -> Dict.insert node.id node.label d) Dict.empty


dictToGraph : Dict NodeId (Force.Entity String) -> Graph (Force.Entity String) () -> Graph (Force.Entity String) ()
dictToGraph d g =
    let
        updateH nodeCtx =
            Dict.get nodeCtx.node.id d
                |> Maybe.withDefault (nodeCtx.node.label)
                |> updateContextWithValue nodeCtx
    in
        Graph.mapContexts updateH g


updateHelp : Msg -> Model -> Model
updateHelp msg ({ drag, graph, simulation } as model) =
    case msg of
        Tick t ->
            let
                ( newState, dict ) =
                    Force.tick simulation (graphToDict graph)
            in
                case drag of
                    Nothing ->
                        Model drag (dictToGraph dict graph) newState

                    Just { current, index } ->
                        Model drag (Graph.update index (Maybe.map (updateNode current)) (dictToGraph dict graph)) newState

        DragStart index xy ->
            Model (Just (Drag xy xy index)) graph simulation

        DragAt xy ->
            case drag of
                Just { start, index } ->
                    Model (Just (Drag start xy index)) (Graph.update index (Maybe.map (updateNode xy)) graph) (Force.reheat simulation)

                Nothing ->
                    Model Nothing graph simulation

        DragEnd xy ->
            case drag of
                Just { start, index } ->
                    Model Nothing (Graph.update index (Maybe.map (updateNode xy)) graph) simulation

                Nothing ->
                    Model Nothing graph simulation


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            -- This allows us to save resources, as if the simulation is done, there is no point in subscribing
            -- to the rAF.
            if Force.isCompleted model.simulation then
                Sub.none
            else
                AnimationFrame.times Tick

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd, AnimationFrame.times Tick ]


onMouseDown : NodeId -> Attribute Msg
onMouseDown index =
    on "mousedown" (Decode.map (DragStart index) Mouse.position)


linkElement graph edge =
    let
        source =
            Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

        target =
            Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.to graph
    in
        line
            [ strokeWidth "1"
            , stroke "#aaa"
            , x1 (toString source.x)
            , y1 (toString source.y)
            , x2 (toString target.x)
            , y2 (toString target.y)
            ]
            []


nodeElement node =
    circle
        [ r "2.5"
        , fill "#000"
        , onMouseDown node.id
        , cx (toString node.label.x)
        , cy (toString node.label.y)
        ]
        [ Svg.title [] [ text node.label.value ] ]


view : Model -> Svg Msg
view model =
    svg [ width "100%", height "100%" ]
        [ g [ class "links" ] <| List.map (linkElement model.graph) <| Graph.edges model.graph
        , g [ class "nodes" ] <| List.map nodeElement <| Graph.nodes model.graph
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

module ForceDirectedGraph exposing (main)

import AnimationFrame
import Dict exposing (Dict)
import Graph exposing (Edge, Graph, Node, NodeId)
import Html
import Html.Events exposing (on)
import Json.Decode as Decode
import Mouse exposing (Position)
import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)
import Time exposing (Time)
import Visualization.Force as Force


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- list of nodes
-- MODEL


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
        nodes =
            Force.initialPositions
                [ Force.entity "Node 1"
                , Force.entity "Node 2"
                , Force.entity "Node 3"
                , Force.entity "Node 3"
                , Force.entity "Node 3"
                , Force.entity "Node 3"
                , Force.entity "Node 3"
                , Force.entity "Node 3"
                , Force.entity "Node 3"
                , Force.entity "Node 3"
                ]

        graph =
            Graph.fromNodeLabelsAndEdgePairs nodes [ ( 0, 1 ), ( 0, 2 ), ( 1, 5 ) ]
    in
        ( Model Nothing graph (Force.simulation [ Force.Center 100 100 ]), Cmd.none )



-- UPDATE


type Msg
    = DragStart NodeId Position
    | DragAt Position
    | DragEnd Position
    | Tick Time


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
        Tick _ ->
            let
                ( newState, dict ) =
                    Force.tick simulation (graphToDict graph)
            in
                Model drag (dictToGraph dict graph) newState

        -- model
        DragStart index xy ->
            Model (Just (Drag xy xy index)) graph simulation

        DragAt xy ->
            case drag of
                Just { start, index } ->
                    Model (Just (Drag start xy index)) (Graph.update index (Maybe.map (updateNode xy)) graph) simulation

                Nothing ->
                    Model Nothing graph simulation

        DragEnd xy ->
            case drag of
                Just { start, index } ->
                    Model Nothing (Graph.update index (Maybe.map (updateNode xy)) graph) simulation

                Nothing ->
                    Model Nothing graph simulation



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            AnimationFrame.diffs Tick

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd, AnimationFrame.diffs Tick ]



-- VIEW


onMouseDown : NodeId -> Attribute Msg
onMouseDown index =
    on "mousedown" (Decode.map (DragStart index) Mouse.position)


linkElement graph edge =
    let
        source =
            Maybe.withDefault (Force.entity "") <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

        target =
            Maybe.withDefault (Force.entity "") <| Maybe.map (.node >> .label) <| Graph.get edge.to graph
    in
        line
            [ strokeWidth "1"
              --(toString (sqrt link.value))
            , stroke "#000"
            , x1 (toString source.x)
            , y1 (toString source.y)
            , x2 (toString target.x)
            , y2 (toString target.y)
            ]
            []


nodeElement node =
    circle
        [ r "5"
        , fill "#000"
          -- , fill (computeColor node.group)
          -- , Attr.title node.id
        , onMouseDown node.id
        , cx (toString node.label.x)
        , cy (toString node.label.y)
        ]
        []


view : Model -> Svg Msg
view model =
    svg [ width "100%", height "100%" ]
        [ g [ class "links" ] <| List.map (linkElement model.graph) <| Graph.edges model.graph
        , g [ class "nodes" ] <| List.map nodeElement <| Graph.nodes model.graph
        ]


computeColor a =
    "#000"

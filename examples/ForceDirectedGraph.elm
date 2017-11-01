module ForceDirectedGraph exposing (main)

{-| This demonstrates laying out the characters in Les Miserables
based on their co-occurence in a scene. Try dragging the nodes!
-}

import AnimationFrame
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html
import Html.Events exposing (on)
import Json.Decode as Decode
import Mouse exposing (Position)
import SampleData exposing (miserablesGraph)
import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)
import Time exposing (Time)
import Visualization.Force as Force exposing (State)


screenWidth : Float
screenWidth =
    990


screenHeight : Float
screenHeight =
    504


type Msg
    = DragStart NodeId Position
    | DragAt Position
    | DragEnd Position
    | Tick Time


type alias Model =
    { drag : Maybe Drag
    , graph : Graph Entity ()
    , simulation : Force.State NodeId
    }


type alias Drag =
    { start : Position
    , current : Position
    , index : NodeId
    }


type alias Entity =
    Force.Entity NodeId { value : String }


init : ( Model, Cmd Msg )
init =
    let
        graph =
            Graph.mapContexts
                (\({ node } as ctx) ->
                    { ctx | node = { label = Force.entity node.id node.label, id = node.id } }
                )
                miserablesGraph

        link { from, to } =
            ( from, to )

        forces =
            [ Force.links <| List.map link <| Graph.edges graph
            , Force.manyBody <| List.map .id <| Graph.nodes graph
            , Force.center (screenWidth / 2) (screenHeight / 2)
            ]
    in
        ( Model Nothing graph (Force.simulation forces), Cmd.none )


updateNode : Position -> NodeContext Entity () -> NodeContext Entity ()
updateNode pos nodeCtx =
    let
        nodeValue =
            nodeCtx.node.label
    in
        updateContextWithValue nodeCtx { nodeValue | x = toFloat pos.x, y = toFloat pos.y }


updateContextWithValue : NodeContext Entity () -> Entity -> NodeContext Entity ()
updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
        { nodeCtx | node = { node | label = value } }


updateGraphWithList : Graph Entity () -> List Entity -> Graph Entity ()
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
        List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


update : Msg -> Model -> Model
update msg ({ drag, graph, simulation } as model) =
    case msg of
        Tick t ->
            let
                ( newState, list ) =
                    Force.tick simulation <| List.map .label <| Graph.nodes graph
            in
                case drag of
                    Nothing ->
                        Model drag (updateGraphWithList graph list) newState

                    Just { current, index } ->
                        Model drag (Graph.update index (Maybe.map (updateNode current)) (updateGraphWithList graph list)) newState

        DragStart index xy ->
            Model (Just (Drag xy xy index)) graph simulation

        DragAt xy ->
            case drag of
                Just { start, index } ->
                    Model (Just (Drag start xy index))
                        (Graph.update index (Maybe.map (updateNode xy)) graph)
                        (Force.reheat simulation)

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
        , stroke "transparent"
        , strokeWidth "7px"
        , onMouseDown node.id
        , cx (toString node.label.x)
        , cy (toString node.label.y)
        ]
        [ Svg.title [] [ text node.label.value ] ]


view : Model -> Svg Msg
view model =
    svg [ width (toString screenWidth ++ "px"), height (toString screenHeight ++ "px") ]
        [ g [ class "links" ] <| List.map (linkElement model.graph) <| Graph.edges model.graph
        , g [ class "nodes" ] <| List.map nodeElement <| Graph.nodes model.graph
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }



{- {"delay": 5001} -}

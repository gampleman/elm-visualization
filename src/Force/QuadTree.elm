module Force.QuadTree exposing (Config, QuadTree(..), Quadrant(..), UserCoords, fromList, performAggregate)

{-| A quadtree that can store an aggregate in the nodes.
Intended for use in n-body simulation, specifically Barnes-Hut

<https://en.wikipedia.org/wiki/Barnes%E2%80%93Hut_simulation#Calculating_the_force_acting_on_a_body>

-}

import BoundingBox2d exposing (BoundingBox2d)
import Point2d exposing (Point2d)
import Units.Pixels exposing (Pixels)
import Units.Quantity as Quantity


type UserCoords
    = UserCoords Never


type QuadTree aggregate item
    = Empty
    | Leaf { boundingBox : BoundingBox2d Pixels UserCoords, aggregate : aggregate, children : ( item, List item ) }
    | Node
        { boundingBox : BoundingBox2d Pixels UserCoords
        , aggregate : aggregate
        , nw : QuadTree aggregate item
        , ne : QuadTree aggregate item
        , se : QuadTree aggregate item
        , sw : QuadTree aggregate item
        }


type alias Config aggregate vertex =
    { combineVertices : vertex -> List vertex -> aggregate
    , combineAggregates : aggregate -> List aggregate -> aggregate
    , toPoint : vertex -> Point2d Pixels UserCoords
    }


{-| An empty aggregate tree
-}
empty : QuadTree aggregate item
empty =
    Empty


{-| A singleton tree, using () as the aggregate
-}
singleton : (vertex -> Point2d Pixels UserCoords) -> vertex -> QuadTree () vertex
singleton toPoint vertex =
    Leaf
        { boundingBox = BoundingBox2d.singleton (toPoint vertex)
        , children = ( vertex, [] )
        , aggregate = ()
        }


insertBy : (vertex -> Point2d Pixels UserCoords) -> vertex -> QuadTree () vertex -> QuadTree () vertex
insertBy toPoint vertex qtree =
    case qtree of
        Empty ->
            Leaf
                { boundingBox = BoundingBox2d.singleton (toPoint vertex)
                , children = ( vertex, [] )
                , aggregate = ()
                }

        Leaf leaf ->
            let
                ( first, rest ) =
                    leaf.children

                newSize =
                    2 + List.length rest

                -- splitting at 32 items seems to be a good choice.
                -- normally, all vertices are put into one large leaf. When it reaches this size
                -- the leaf is split. From then on the node will resize when it encounters a point outside of its bounding box.
                -- resizing is expensive, so we wait a while before splitting. On the other hand, if we never split at all,
                -- we get none of the benefits of using a quadtree.
                maxSize =
                    32
            in
            if newSize >= maxSize then
                let
                    initial =
                        Node
                            { boundingBox = BoundingBox2d.union leaf.boundingBox (BoundingBox2d.singleton (toPoint vertex))
                            , ne = Empty
                            , se = Empty
                            , nw = Empty
                            , sw = Empty
                            , aggregate = ()
                            }
                in
                List.foldl (insertBy toPoint) initial (first :: rest)

            else
                Leaf
                    { boundingBox = BoundingBox2d.union leaf.boundingBox (BoundingBox2d.singleton (toPoint vertex))
                    , children = ( vertex, first :: rest )
                    , aggregate = ()
                    }

        Node node ->
            let
                point =
                    toPoint vertex
            in
            if BoundingBox2d.contains point node.boundingBox then
                -- NOTE: writing out the full records here gives ~50% speed increase
                case quadrant node.boundingBox point of
                    NE ->
                        Node
                            { boundingBox = node.boundingBox
                            , aggregate = node.aggregate
                            , ne = insertBy toPoint vertex node.ne
                            , se = node.se
                            , nw = node.nw
                            , sw = node.sw
                            }

                    SE ->
                        Node
                            { boundingBox = node.boundingBox
                            , aggregate = node.aggregate
                            , ne = node.ne
                            , se = insertBy toPoint vertex node.se
                            , nw = node.nw
                            , sw = node.sw
                            }

                    NW ->
                        Node
                            { boundingBox = node.boundingBox
                            , aggregate = node.aggregate
                            , ne = node.ne
                            , se = node.se
                            , nw = insertBy toPoint vertex node.nw
                            , sw = node.sw
                            }

                    SW ->
                        Node
                            { boundingBox = node.boundingBox
                            , aggregate = node.aggregate
                            , ne = node.ne
                            , se = node.se
                            , nw = node.nw
                            , sw = insertBy toPoint vertex node.sw
                            }

            else
                let
                    { minX, minY, maxX, maxY } =
                        BoundingBox2d.extrema node.boundingBox

                    ( width, height ) =
                        BoundingBox2d.dimensions node.boundingBox
                in
                case quadrant node.boundingBox point of
                    NE ->
                        Node
                            { boundingBox = BoundingBox2d.fromExtrema { minX = minX, maxX = maxX |> Quantity.plus width, minY = minY, maxY = maxY |> Quantity.plus height }
                            , ne = singleton toPoint vertex
                            , sw = qtree
                            , se = Empty
                            , nw = Empty
                            , aggregate = ()
                            }

                    SE ->
                        Node
                            { boundingBox = BoundingBox2d.fromExtrema { maxY = maxY, minX = minX, maxX = maxX |> Quantity.plus width, minY = minY |> Quantity.minus height }
                            , se = singleton toPoint vertex
                            , nw = qtree
                            , sw = Empty
                            , ne = Empty
                            , aggregate = ()
                            }

                    NW ->
                        Node
                            { boundingBox = BoundingBox2d.fromExtrema { maxX = maxX, minY = minY, maxY = maxY |> Quantity.plus height, minX = minX |> Quantity.minus width }
                            , nw = singleton toPoint vertex
                            , se = qtree
                            , sw = Empty
                            , ne = Empty
                            , aggregate = ()
                            }

                    SW ->
                        Node
                            { boundingBox = BoundingBox2d.fromExtrema { maxX = maxX, maxY = maxY, minX = minX |> Quantity.minus width, minY = minY |> Quantity.minus height }
                            , sw = singleton toPoint vertex
                            , ne = qtree
                            , se = Empty
                            , nw = Empty
                            , aggregate = ()
                            }


type Quadrant
    = NE
    | NW
    | SE
    | SW


quadrant : BoundingBox2d Pixels UserCoords -> Point2d Pixels UserCoords -> Quadrant
quadrant boundingBox point =
    let
        ( midX, midY ) =
            BoundingBox2d.centerPoint boundingBox |> Point2d.coordinates

        ( x, y ) =
            Point2d.coordinates point
    in
    if y |> Quantity.greaterThanOrEqualTo midY then
        if x |> Quantity.greaterThanOrEqualTo midX then
            NE

        else
            NW

    else if x |> Quantity.greaterThanOrEqualTo midX then
        SE

    else
        SW


fromList : (vertex -> Point2d Pixels UserCoords) -> List vertex -> QuadTree () vertex
fromList toPoint =
    List.foldl (insertBy toPoint) empty


performAggregate : Config aggregate vertex -> QuadTree x vertex -> QuadTree aggregate vertex
performAggregate ({ combineAggregates, combineVertices } as config) vanillaQuadTree =
    case vanillaQuadTree of
        Empty ->
            Empty

        Leaf leaf ->
            let
                ( first, rest ) =
                    leaf.children
            in
            Leaf
                { boundingBox = leaf.boundingBox
                , children = ( first, rest )
                , aggregate = combineVertices first rest
                }

        Node node ->
            let
                newNw =
                    performAggregate config node.nw

                newSw =
                    performAggregate config node.sw

                newNe =
                    performAggregate config node.ne

                newSe =
                    performAggregate config node.se

                subresults =
                    List.filterMap getAggregate [ newNw, newSw, newNe, newSe ]
            in
            case subresults of
                [] ->
                    Empty

                x :: xs ->
                    Node
                        { boundingBox = node.boundingBox
                        , aggregate = combineAggregates x xs
                        , nw = newNw
                        , sw = newSw
                        , ne = newNe
                        , se = newSe
                        }


getAggregate : QuadTree aggregate vertex -> Maybe aggregate
getAggregate qtree =
    case qtree of
        Empty ->
            Nothing

        Leaf { aggregate } ->
            Just aggregate

        Node { aggregate } ->
            Just aggregate

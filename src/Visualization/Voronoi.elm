module Visualization.Voronoi exposing (..)

import Array exposing (Array)
import List.Extra
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Polygon2d as Polygon2d exposing (Polygon2d, vertices)
import OpenSolid.Svg exposing (polygon2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Svg exposing (g, svg)
import Svg.Attributes exposing (fill, stroke)


type alias Point =
    ( Float, Float )


type alias Site =
    Point



-- Diagram


type Vertex
    = OrdinaryPoint Point2d
    | PointAtInfinity Vector2d


{-| This is internally stored as a winged-edge graph.
-}
type Diagram
    = Diagram
        { edgeAroundVertex : Array Int
        , edgeAroundPolygon : Array Int
        , rightPolygon : Array Int
        , leftPolygon : Array Int
        , startVertex : Array Int
        , endVertex : Array Int
        , cwPredecessor : Array Int
        , ccwPredecessor : Array Int
        , cwSuccessor : Array Int
        , ccwSuccessor : Array Int
        , vertices : Array Vertex
        }


polygons : BoundingBox2d -> Diagram -> List Polygon2d
polygons bbox ((Diagram { edgeAroundPolygon }) as diagram) =
    let
        pipeline index edge =
            verticesSuroundingPolygonStartingWithEdge diagram index edge
                |> Maybe.withDefault []
                |> List.map (retrieveCoordinate bbox diagram >> Maybe.withDefault Point2d.origin)
                |> Polygon2d.fromVertices
    in
        Array.indexedMap pipeline edgeAroundPolygon
            |> Array.toList


isFinite : Maybe Vertex -> Bool
isFinite p =
    case p of
        Just (OrdinaryPoint point) ->
            True

        _ ->
            False


intersectionPoint : BoundingBox2d -> Vector2d -> Point2d -> Point2d
intersectionPoint bbox vector2d point2d =
    let
        { minX, maxX, minY, maxY } =
            BoundingBox2d.extrema bbox

        ( w, h ) =
            BoundingBox2d.dimensions bbox

        l =
            sqrt (w ^ 2 + h ^ 2)

        ( slx, sly ) =
            Vector2d.components vector2d

        m =
            sly / slx

        ( x, y ) =
            Point2d.coordinates point2d

        minXy =
            (if slx == 0 then
                (minX - x) + y
             else
                m * (minX - x) + y
            )

        maxXy =
            (if slx == 0 then
                (maxX - x) + y
             else
                m * (maxX - x) + y
            )

        minYx =
            (if slx == 0 then
                x
             else
                (minY - y) / m + x
            )

        maxYx =
            (if slx == 0 then
                x
             else
                (maxY - y) / m + x
            )
    in
        if x <= BoundingBox2d.midX bbox && minY <= minXy && minXy <= maxY then
            Point2d.fromCoordinates ( minX, minXy )
        else if x >= BoundingBox2d.midX bbox && minY <= maxXy && maxXy <= maxY then
            Point2d.fromCoordinates ( maxX, maxXy )
        else if y <= BoundingBox2d.midY bbox && minX <= minYx && minYx <= maxX then
            Point2d.fromCoordinates ( minYx, minY )
        else if y <= BoundingBox2d.midY bbox && minX <= maxYx && maxYx <= maxX then
            Point2d.fromCoordinates ( maxYx, maxY )
        else
            Point2d.fromCoordinates ( x, y ) viridis


retrieveCoordinate : BoundingBox2d -> Diagram -> Int -> List Point2d
retrieveCoordinate bbox ((Diagram { vertices, startVertex, endVertex }) as diagram) index =
    Array.get index vertices
        |> Maybe.map
            (\vertex ->
                case vertex of
                    OrdinaryPoint point ->
                        [ point ]

                    PointAtInfinity vector ->
                        let
                            filter : ( Maybe Int, Maybe Int ) -> Maybe Point2d
                            filter ( startIndex, endIndex ) =
                                let
                                    start =
                                        get vertices startIndex

                                    end =
                                        get vertices endIndex
                                in
                                    if start /= Just vertex && isFinite start then
                                        Maybe.andThen finite start
                                    else if end /= Just vertex && isFinite end then
                                        Maybe.andThen finite end
                                    else
                                        Nothing
                        in
                            edgesIncidentToAVertex index diagram
                                |> Maybe.withDefault []
                                |> List.map (\edge -> ( Array.get edge startVertex, Array.get edge endVertex ))
                                |> List.filterMap filter
                                |> List.head
                                |> Maybe.withDefault Point2d.origin
                                |> intersectionPoint bbox vector
            )
        |> Maybe.withDefault []


finite : Vertex -> Maybe Point2d
finite vertex =
    case vertex of
        OrdinaryPoint point2d ->
            Just point2d

        PointAtInfinity vector2d ->
            Nothing


verticesSuroundingPolygonStartingWithEdge : Diagram -> Int -> Int -> Maybe (List Int)
verticesSuroundingPolygonStartingWithEdge ((Diagram { leftPolygon, cwSuccessor, cwPredecessor, endVertex, startVertex }) as diagram) i kStart =
    let
        helper k acc =
            let
                ( next, app ) =
                    if Just i == get leftPolygon k then
                        ( get cwSuccessor k, get endVertex k )
                    else
                        ( get cwPredecessor k, get startVertex k )
            in
                if next == (Just kStart) then
                    Maybe.map (\jk -> jk :: acc) app
                else
                    case app of
                        Just jk ->
                            helper next (jk :: acc)

                        Nothing ->
                            Nothing
    in
        helper (Just kStart) []


get : Array a -> Maybe Int -> Maybe a
get a =
    Maybe.andThen ((flip Array.get) a)


edgesIncidentToAVertex : Int -> Diagram -> Maybe (List Int)
edgesIncidentToAVertex j ((Diagram { edgeAroundVertex, startVertex, ccwPredecessor, ccwSuccessor }) as diagram) =
    let
        kStart =
            Array.get j edgeAroundVertex

        helper k acc =
            let
                next =
                    if Just j == get startVertex k then
                        get ccwPredecessor k
                    else
                        get ccwSuccessor k
            in
                if next == kStart then
                    Maybe.map (\jk -> jk :: acc) k
                else
                    case k of
                        Just jk ->
                            helper next (jk :: acc)

                        Nothing ->
                            Nothing
    in
        helper kStart []


hull : List Point -> Maybe (List Point)
hull points =
    Debug.crash "This is implemented elsewhere"


voronoi : List Site -> Diagram
voronoi =
    List.sort >> Array.fromList >> voronoiHelp


voronoiHelp sites =
    let
        n =
            Array.length sites
    in
        if n < 3 then
            Debug.crash "don't know what to do here"
        else
            merge (voronoiHelp (Array.slice 0 (n // 2) sites)) (voronoiHelp (Array.slice (n // 2) -1 sites))


merge : Diagram -> Diagram -> Diagram
merge left right =
    -- let
    --     lcs =
    --         lowerCommonSupport (hull left) (hull right)
    -- in
    Debug.crash ""


isUnder : LineSegment2d -> Point2d -> Bool
isUnder line point =
    let
        vector1 =
            LineSegment2d.vector line

        vector2 =
            Vector2d.from (LineSegment2d.startPoint line) point
    in
        Vector2d.crossProduct vector1 vector2 > 0


rotateTo : (Float -> Float -> Bool) -> List Point2d -> List Point2d
rotateTo cmp list =
    case list of
        [] ->
            []

        x :: xs ->
            let
                ( index, _ ) =
                    List.Extra.indexedFoldr
                        (\index point ( maxIndex, maxPoint ) ->
                            if cmp (Point2d.xCoordinate point) (Point2d.xCoordinate maxPoint) then
                                ( index, point )
                            else
                                ( maxIndex, maxPoint )
                        )
                        ( 0, x )
                        xs
            in
                List.drop (index + 1) list ++ List.take (index + 1) list


type Which
    = Left
    | Right


lowerCommonSupport : Polygon2d -> Polygon2d -> LineSegment2d
lowerCommonSupport pl pr =
    let
        ( ml, left ) =
            rotateTo (>) (Polygon2d.vertices pl) |> List.Extra.uncons |> Maybe.withDefault ( Point2d.origin, [] )

        ( mr, right ) =
            rotateTo (<) (Polygon2d.vertices pr) |> List.Extra.uncons |> Maybe.withDefault ( Point2d.origin, [] )

        line =
            LineSegment2d.from ml mr
    in
        lowerCommonSupportHelper line Left left (List.reverse right)


lowerCommonSupportHelper : LineSegment2d -> Which -> List Point2d -> List Point2d -> LineSegment2d
lowerCommonSupportHelper line which u v =
    case which of
        Left ->
            case u of
                [] ->
                    line

                x :: xs ->
                    if isUnder line x then
                        lowerCommonSupportHelper (LineSegment2d.from x (LineSegment2d.endPoint line)) Right xs v
                    else
                        line

        Right ->
            case v of
                [] ->
                    line

                x :: xs ->
                    if isUnder line x then
                        lowerCommonSupportHelper (LineSegment2d.from (LineSegment2d.startPoint line) x) Left u xs
                    else
                        line


diagram =
    Diagram
        { edgeAroundVertex = Array.fromList [ 0, 0, 1, 3, 4, 2 ]
        , edgeAroundPolygon = Array.fromList [ 0, 1, 0, 4, 6 ]
        , rightPolygon = Array.fromList [ 0, 0, 1, 3, 2, 4, 4, 4, 4 ]
        , leftPolygon = Array.fromList [ 2, 1, 2, 0, 4, 1, 2, 3, 0 ]
        , startVertex = Array.fromList [ 1, 0, 0, 1, 1, 2, 5, 4, 3 ]
        , endVertex = Array.fromList [ 0, 2, 5, 3, 4, 5, 4, 3, 2 ]
        , cwPredecessor = Array.fromList [ 3, 0, 1, 4, 0, 8, 5, 6, 7 ]
        , ccwPredecessor = Array.fromList [ 4, 2, 0, 0, 3, 1, 2, 4, 3 ]
        , cwSuccessor = Array.fromList [ 2, 5, 6, 8, 7, 2, 4, 3, 1 ]
        , ccwSuccessor = Array.fromList [ 1, 8, 5, 7, 6, 6, 7, 8, 5 ]
        , vertices =
            Array.fromList
                [ OrdinaryPoint (Point2d.fromCoordinates ( 52, 60 ))
                , OrdinaryPoint (Point2d.fromCoordinates ( 85, 80 ))
                , PointAtInfinity (Vector2d.fromComponents ( 0, -1 ))
                , PointAtInfinity (Vector2d.fromComponents ( 1, 0 ))
                , PointAtInfinity (Vector2d.fromComponents ( 0, 1 ))
                , PointAtInfinity (Vector2d.fromComponents ( -0.3, 0.5 ))
                ]
        }


bbox =
    BoundingBox2d.with { minX = 0, maxX = 150, minY = 0, maxY = 150 }


main =
    svg []
        [ g [] (polygons bbox diagram |> List.map (polygon2d [ fill "none", stroke "black" ]))
        ]

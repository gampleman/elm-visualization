module Hierarchy.TidyTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Hierarchy.Tidy
import Hierarchy.Tree as Tree exposing (Tree(..))
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Test exposing (Test)
import Test.Html.Query exposing (children)


suite =
    Test.describe "Aesthetic rules for trees:"
        [ Test.fuzz (fuzzHierarchy 3 3) "Rule 1: No nodes should overlap" <|
            \tree ->
                tree
                    |> doLayout
                    |> expectNoOverlapNodes
        , Test.fuzz (fuzzHierarchy 3 3) "Rule 2: No lines should cross" <|
            \tree ->
                tree
                    |> doLayout
                    |> expectNoCrossedLinks
        , Test.fuzz (fuzzHierarchy 3 3) "Rule 3: A node's children should stay on the same line" <|
            \tree ->
                tree
                    |> doLayout
                    |> expectAllChildrenToHaveSameY
        , Test.fuzz (fuzzHierarchy 3 3) "Rule 6: Nodes are ordered. Drawings of nodes should have the same order" <|
            \tree ->
                tree
                    |> doLayout
                    |> expectNodesToBeOrdered
        ]


fuzzHierarchy : Int -> Int -> Fuzzer (Tree ( Int, Float, Float ))
fuzzHierarchy depth branch =
    if depth > 0 then
        Fuzz.map4
            (\label width height children ->
                Tree.tree ( label, width, height ) children
            )
            Fuzz.int
            (Fuzz.floatRange 1 10)
            (Fuzz.floatRange 1 10)
            (Fuzz.listOfLengthBetween 0 branch (fuzzHierarchy (depth - 1) branch))

    else
        Fuzz.map3
            (\label width height ->
                Tree.singleton ( label, width, height )
            )
            Fuzz.int
            (Fuzz.floatRange 1 10)
            (Fuzz.floatRange 1 10)


type alias FinishedLayout =
    Tree { height : Float, value : ( Int, Float, Float ), width : Float, x : Float, y : Float }


expectNoOverlapNodes : FinishedLayout -> Expectation
expectNoOverlapNodes lay =
    let
        intersects self other =
            self.x
                - self.width
                / 2
                < other.x
                + other.width
                / 2
                && self.x
                + self.width
                / 2
                > other.x
                - other.width
                / 2
                && self.y
                < other.y
                + other.height
                && self.y
                + self.height
                > other.y
    in
    checkAll intersects (\head intersect -> "Expected nodes in \n\n" ++ formatTree lay ++ "\n\nnot to intersect, but found an interesction betweeen \n" ++ Debug.toString head ++ "\n and \n" ++ Debug.toString intersect) (Tree.flatten lay)


checkAll : (a -> a -> Bool) -> (a -> a -> String) -> List a -> Expectation
checkAll test error lst =
    let
        findFirstFail fst lst_ =
            case lst_ of
                [] ->
                    Nothing

                x :: xs ->
                    if test fst x then
                        Just x

                    else
                        findFirstFail fst xs
    in
    case lst of
        [] ->
            Expect.pass

        head :: tail ->
            case findFirstFail head tail of
                Just failed ->
                    Expect.fail (error head failed)

                Nothing ->
                    checkAll test error tail


expectNoCrossedLinks : FinishedLayout -> Expectation
expectNoCrossedLinks lay =
    Tree.links lay
        |> List.map (\( from, to ) -> LineSegment2d.from (Point2d.pixels from.x (from.y + from.height)) (Point2d.pixels to.x to.y))
        |> checkAll (\line1 line2 -> LineSegment2d.intersectionPoint line1 line2 /= Nothing && not (connected line1 line2)) (\line1 line2 -> "Expected lines in \n\n" ++ formatTree lay ++ "\n\nnot to intersect, but found an interesction betweeen \n" ++ Debug.toString line1 ++ "\n and \n" ++ Debug.toString line2 ++ "\n -- " ++ Debug.toString (LineSegment2d.intersectionPoint line1 line2))


connected : LineSegment2d u c -> LineSegment2d u c -> Bool
connected a b =
    LineSegment2d.startPoint a == LineSegment2d.startPoint b || LineSegment2d.startPoint a == LineSegment2d.endPoint b || LineSegment2d.startPoint a == LineSegment2d.endPoint b || LineSegment2d.endPoint a == LineSegment2d.endPoint b


expectAllChildrenToHaveSameY : FinishedLayout -> Expectation
expectAllChildrenToHaveSameY tree =
    case
        Tree.findBfs
            (\t ->
                case Tree.children t of
                    [] ->
                        False

                    headNode :: tail ->
                        List.any (\child -> (Tree.label child).y /= (Tree.label headNode).y) tail
            )
            tree
    of
        Nothing ->
            Expect.pass

        Just subTree ->
            Expect.fail ("Expected all nodes in " ++ formatTree tree ++ " to have the same Y coordinate.\nBut the children of this subtree have differing y coordinates: " ++ formatTree subTree)


expectNodesToBeOrdered : FinishedLayout -> Expectation
expectNodesToBeOrdered tree =
    case
        Tree.findBfs
            (\t ->
                case Tree.children t of
                    [] ->
                        False

                    headNode :: tail ->
                        List.foldl (\child ( prevX, badSoFar ) -> ( (Tree.label child).x, badSoFar || (Tree.label child).x <= prevX )) ( (Tree.label headNode).x, False ) tail
                            |> Tuple.second
            )
            tree
    of
        Nothing ->
            Expect.pass

        Just subtree ->
            Expect.fail ("Expected all nodes in " ++ formatTree tree ++ " to be ordered. But the child nodes of " ++ formatTree subtree ++ "are out of order.")



-- expect


doLayout =
    Hierarchy.Tidy.layout { width = \( _, w, _ ) -> w, height = \( _, _, h ) -> h, layered = False, parentChildMargin = 1, peerMargin = 1 }


formatTree : FinishedLayout -> String
formatTree =
    let
        go indent tree =
            String.join "\n" ((String.repeat (indent * 2) " " ++ Debug.toString (Tree.label tree)) :: List.map (go (indent + 1)) (Tree.children tree))
    in
    go 0


test1 =
    -- Test.only <|
    Test.test "Rule 1" <|
        \() ->
            Tree.tree ( 0, 1, 1 ) [ Tree.singleton ( 0, 1, 1 ), Tree.tree ( 0, 1, 1 ) [ Tree.singleton ( 0, 1, 1 ) ], Tree.singleton ( 0, 1, 1 ) ]
                |> doLayout
                |> expectNoOverlapNodes


test2 =
    -- Test.only <|
    Test.test "Rule 2" <|
        \() ->
            Tree.tree ( 0, 8, 7 )
                [ Tree.tree ( 1, 3, 9 )
                    [ Tree.singleton ( 10, 3, 8 )
                    , Tree.singleton ( 10, 5, 5 )
                    , Tree.singleton ( 10, 6, 8 )
                    ]
                , Tree.singleton ( 3, 1, 1 )
                ]
                |> doLayout
                |> expectNoOverlapNodes


test3 =
    -- Test.only <|
    Test.test "Rule 1 case 3:" <|
        \() ->
            Tree.tree ( 0, 1, 1 ) [ Tree.tree ( 0, 1, 1 ) [ Tree.tree ( 0, 1, 10 ) [ Tree.singleton ( 0, 1, 1 ), Tree.singleton ( 0, 1, 1 ) ], Tree.tree ( 0, 1, 1 ) [ Tree.singleton ( 0, 1, 1 ) ], Tree.tree ( 0, 1, 10 ) [ Tree.singleton ( 0, 10, 1 ) ] ] ]
                |> doLayout
                |> expectNoOverlapNodes

module Hierarchy.TidyTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Hierarchy exposing (Hierarchy(..))
import Hierarchy.Tidy
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


fuzzHierarchy : Int -> Int -> Fuzzer (Hierarchy ( Int, Float, Float ))
fuzzHierarchy depth branch =
    if depth > 0 then
        Fuzz.map4
            (\label width height children ->
                Hierarchy ( label, width, height ) children
            )
            Fuzz.int
            (Fuzz.floatRange 1 10)
            (Fuzz.floatRange 1 10)
            (Fuzz.listOfLengthBetween 0 branch (fuzzHierarchy (depth - 1) branch))

    else
        Fuzz.map3
            (\label width height ->
                Hierarchy ( label, width, height ) []
            )
            Fuzz.int
            (Fuzz.floatRange 1 10)
            (Fuzz.floatRange 1 10)


type alias FinishedLayout =
    Hierarchy { height : Float, value : ( Int, Float, Float ), width : Float, x : Float, y : Float, relativeX : Float, modifierToSubtree : Float }


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
    checkAll intersects (\head intersect -> "Expected nodes in \n\n" ++ formatTree lay ++ "\n\nnot to intersect, but found an interesction betweeen \n" ++ Debug.toString head ++ "\n and \n" ++ Debug.toString intersect) (Hierarchy.toList lay)


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
    Hierarchy.Tidy.links lay
        |> List.map (\( from, to ) -> LineSegment2d.from (Point2d.pixels from.x (from.y + from.height)) (Point2d.pixels to.x to.y))
        |> checkAll (\line1 line2 -> LineSegment2d.intersectionPoint line1 line2 /= Nothing && not (connected line1 line2)) (\line1 line2 -> "Expected lines in \n\n" ++ formatTree lay ++ "\n\nnot to intersect, but found an interesction betweeen \n" ++ Debug.toString line1 ++ "\n and \n" ++ Debug.toString line2 ++ "\n -- " ++ Debug.toString (LineSegment2d.intersectionPoint line1 line2))


connected : LineSegment2d u c -> LineSegment2d u c -> Bool
connected a b =
    LineSegment2d.startPoint a == LineSegment2d.startPoint b || LineSegment2d.startPoint a == LineSegment2d.endPoint b || LineSegment2d.startPoint a == LineSegment2d.endPoint b || LineSegment2d.endPoint a == LineSegment2d.endPoint b


expectAllChildrenToHaveSameY : FinishedLayout -> Expectation
expectAllChildrenToHaveSameY tree =
    let
        go (Hierarchy _ children) =
            case children of
                [] ->
                    True

                ((Hierarchy head _) as headNode) :: tail ->
                    go headNode && List.all (\((Hierarchy node _) as child) -> node.y == head.y && go child) tail
    in
    if go tree then
        Expect.pass

    else
        Expect.fail ("Expected all nodes in " ++ formatTree tree ++ " to have the same Y coordinate")


expectNodesToBeOrdered : FinishedLayout -> Expectation
expectNodesToBeOrdered tree =
    let
        go (Hierarchy _ children) =
            case children of
                [] ->
                    True

                ((Hierarchy head _) as headNode) :: tail ->
                    List.foldl (\((Hierarchy node _) as child) ( prevX, goodSoFar ) -> ( node.x, goodSoFar && node.x > prevX && go child )) ( head.x, go headNode ) tail
                        |> Tuple.second
    in
    if go tree then
        Expect.pass

    else
        Expect.fail ("Expected all nodes in " ++ formatTree tree ++ " to be ordered")



-- expect


doLayout =
    Hierarchy.Tidy.layout { width = \( _, w, _ ) -> w, height = \( _, _, h ) -> h, layered = False, parentChildMargin = 1, peerMargin = 1 }


formatTree : FinishedLayout -> String
formatTree =
    let
        go indent (Hierarchy node children) =
            String.join "\n" ((String.repeat (indent * 2) " " ++ Debug.toString node) :: List.map (go (indent + 1)) children)
    in
    go 0


test1 =
    -- Test.only <|
    Test.test "Rule 1" <|
        \() ->
            Hierarchy ( 0, 1, 1 ) [ Hierarchy ( 0, 1, 1 ) [], Hierarchy ( 0, 1, 1 ) [ Hierarchy ( 0, 1, 1 ) [] ], Hierarchy ( 0, 1, 1 ) [] ]
                |> doLayout
                |> expectNoOverlapNodes


test2 =
    -- Test.only <|
    Test.test "Rule 2" <|
        \() ->
            Hierarchy ( 0, 8, 7 )
                [ Hierarchy ( 1, 3, 9 )
                    [ Hierarchy ( 10, 3, 8 ) []
                    , Hierarchy ( 10, 5, 5 ) []
                    , Hierarchy ( 10, 6, 8 ) []
                    ]
                , Hierarchy ( 3, 1, 1 ) []
                ]
                |> doLayout
                |> expectNoOverlapNodes


test3 =
    -- Test.only <|
    Test.test "Rule 1 case 3:" <|
        \() ->
            Hierarchy ( 0, 1, 1 ) [ Hierarchy ( 0, 1, 1 ) [ Hierarchy ( 0, 1, 10 ) [ Hierarchy ( 0, 1, 1 ) [], Hierarchy ( 0, 1, 1 ) [] ], Hierarchy ( 0, 1, 1 ) [ Hierarchy ( 0, 1, 1 ) [] ], Hierarchy ( 0, 1, 10 ) [ Hierarchy ( 0, 10, 1 ) [] ] ] ]
                |> doLayout
                |> expectNoOverlapNodes

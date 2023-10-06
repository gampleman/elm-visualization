module Hierarchy.Tidy exposing (layout)

{-| Based on <https://github.com/zxch3n/tidy/blob/master/rust/crates/tidy-tree/src/layout/tidy_layout.rs>.

For a description, see <https://www.zxch3n.com/tidy/tidy/>.

-}

import Array exposing (Array)
import Dict
import Tree exposing (Tree)



--- Debug
-- debugTree : String -> TidyLayout a -> TidyLayout a
-- debugTree prefix ((TidyLayout { tree }) as lay) =
--     let
--         go indent (Tree node children) =
--             case derefTidyData lay node.id of
--                 Just tidy ->
--                     String.join "\n" ((String.repeat (indent * 2) " " ++ Debug.toString tidy) :: List.map (go (indent + 1)) children)
--                 Nothing ->
--                     ""
--         _ =
--             Debug.log (prefix ++ ": \n" ++ go 0 tree) ()
--     in
--     lay
--
-- logLayoutProperty : String -> Int -> (TidyData -> a) -> TidyLayout b -> TidyLayout b
-- logLayoutProperty label id getter lay =
--     let
--         _ =
--             case derefTidyData lay id of
--                 Just d ->
--                     Debug.log label (getter d)
--                 Nothing ->
--                     Debug.todo (label ++ "property not found")
--     in
--     lay


type alias Id =
    Int



-- This code uses a flat array to represent the tree data, where each node has an ID
-- which is also it's index into this array.
-- The reason for this is that this is not really just a tree, but a graph, where different
-- nodes need access to other nodes in the hierarchy, plus we need to be able to both
-- read and write reasonably efficiently while traversing the datastructure.


type alias TidyLayout a =
    Array (TidyData a)


type alias TidyData a =
    { threadLeft : Id
    , threadRight : Id
    , -- this.extreme_left == this.thread_left.extreme_left ||
      -- this.extreme_left == this.children[0].extreme_left
      extremeLeft : Id
    , -- this.extreme_right == this.thread_right.extreme_right ||
      -- this.extreme_right == this.children[-1].extreme_right
      extremeRight : Id
    , -- Cached change of x position.
      shiftAcceleration : Float
    , -- Cached change of x position
      shiftChange : Float
    , -- this.x = parent.x + modifier_to_subtree
      modifierToSubtree : Float
    , -- this.x + modifier_thread_left == thread_left.x
      modifierThreadLeft : Float
    , -- this.x + modifier_thread_right == thread_right.x
      modifierThreadRight : Float
    , -- this.x + modifier_extreme_left == extreme_left.x
      modifierExtremeLeft : Float
    , -- this.x + modifier_extreme_right == extreme_right.x
      modifierExtremeRight : Float
    , x : Float
    , y : Float
    , -- node x position relative to its parent
      relativeX : Float
    , -- node y position relative to its parent
      relativeY : Float

    -- copied for speed
    , width : Float
    , height : Float
    , children : Array Id
    , id : Id
    , value : a
    }


traverseWithParent : ({ parent : Maybe (TidyData a), node : TidyData a } -> TidyData a) -> TidyLayout a -> TidyLayout a
traverseWithParent fn l =
    let
        help maybeParent val lay =
            let
                newL =
                    updateTidyData
                        (\tidy ->
                            fn
                                { parent =
                                    Maybe.andThen
                                        (\parent ->
                                            derefTidyData lay parent.id
                                         -- |> Maybe.map (\d -> { node = parent, tidy = d })
                                        )
                                        maybeParent
                                , node = tidy
                                }
                        )
                        val.id
                        lay
            in
            List.foldl (help (Just val)) newL (derefChildren l val)
    in
    case derefTidyData l 0 of
        Just root ->
            help Nothing root l

        Nothing ->
            l


traverseBFSWithDepth : (Int -> a -> TidyData b -> ( a, TidyData b )) -> a -> TidyLayout b -> TidyLayout b
traverseBFSWithDepth fn init lay =
    let
        help accu front back l =
            case front of
                [] ->
                    case back of
                        [] ->
                            l

                        _ ->
                            help accu (List.reverse back) [] l

                ( depth, x ) :: xs ->
                    case derefTidyData l x of
                        Just node ->
                            let
                                ( newAccu, newNode ) =
                                    fn depth accu node
                            in
                            help newAccu xs (List.map (Tuple.pair (depth + 1)) (List.reverse (Array.toList newNode.children)) ++ back) (Array.set x newNode l)

                        Nothing ->
                            help accu xs back l
    in
    help init [ ( 0, 0 ) ] [] lay


initialize : (a -> ( Float, Float )) -> Tree a -> TidyLayout a
initialize nodeSize tree =
    Tree.depthFirstFold
        (\lst _ node children ->
            let
                ( w, h ) =
                    nodeSize (Tuple.second node)
            in
            Tree.Continue
                ({ threadLeft = -1
                 , threadRight = -1
                 , extremeLeft = -1
                 , extremeRight = -1
                 , shiftAcceleration = 0
                 , shiftChange = 0
                 , modifierToSubtree = 0
                 , modifierThreadLeft = 0
                 , modifierThreadRight = 0
                 , modifierExtremeLeft = 0
                 , modifierExtremeRight = 0
                 , x = 0
                 , y = 0
                 , relativeX = 0
                 , relativeY = 0
                 , width = w
                 , height = h

                 -- TODO: there is an interesting optimization possible here:
                 -- TODO: if the IDs were assigned in a BFS sort of way, then
                 -- TODO: each nodes children's ids would be consecutive integers
                 -- TODO: and could be runlength encoded (i.e. startIndex + length)
                 -- TODO: which would make this datastructure static in memory size
                 -- TODO: and potentially faster to traverse...
                 , children = Array.fromList (List.map (Tree.label >> Tuple.first) children)
                 , id = Tuple.first node
                 , value = Tuple.second node
                 }
                    :: lst
                )
        )
        []
        (Tree.indexedMap Tuple.pair tree)
        -- TODO: If the IDs were assigned in the same order as the subsequent map,
        -- TODO: then the sort here would be unnecessary
        |> List.sortBy .id
        |> Array.fromList


derefTidyData : TidyLayout a -> Int -> Maybe (TidyData a)
derefTidyData l id =
    Array.get id l


updateTidyData : (TidyData a -> TidyData a) -> Int -> TidyLayout a -> TidyLayout a
updateTidyData fn id l =
    case Array.get id l of
        Just td ->
            Array.set id (fn td) l

        Nothing ->
            l


derefChildren : TidyLayout a -> TidyData a -> List (TidyData a)
derefChildren l node =
    node.children
        |> Array.toList
        |> List.filterMap (derefTidyData l)


derefBottom : Int -> TidyLayout a -> Float
derefBottom id l =
    derefTidyData l id
        |> Maybe.map (\{ height, y } -> height + y)
        |> Maybe.withDefault 0


firstChild : Id -> TidyLayout a -> Maybe (TidyData a)
firstChild idx lay =
    derefTidyData lay idx
        |> Maybe.andThen
            (\td ->
                Array.get 0 td.children
            )
        |> Maybe.andThen (derefTidyData lay)


lastChild : Id -> TidyLayout a -> Maybe (TidyData a)
lastChild idx lay =
    derefTidyData lay idx
        |> Maybe.andThen
            (\td ->
                Array.get (Array.length td.children - 1) td.children
            )
        |> Maybe.andThen (derefTidyData lay)


setExtreme : Id -> TidyLayout a -> TidyLayout a
setExtreme id tidyLayout =
    updateTidyData
        (\tidy ->
            case
                Maybe.map2 Tuple.pair
                    (firstChild id tidyLayout)
                    (lastChild id tidyLayout)
            of
                Nothing ->
                    { tidy | extremeLeft = tidy.id, extremeRight = tidy.id, modifierExtremeLeft = 0, modifierExtremeRight = 0 }

                Just ( first, last ) ->
                    { tidy | extremeLeft = first.extremeLeft, extremeRight = last.extremeRight, modifierExtremeLeft = first.modifierToSubtree + first.modifierExtremeLeft, modifierExtremeRight = last.modifierToSubtree + last.modifierExtremeRight }
        )
        id
        tidyLayout


type alias YList =
    List { index : Int, id : Int, y : Float }


moveSubtree : Int -> Maybe { y : Float, id : Int, index : Int } -> Int -> Float -> TidyLayout a -> TidyLayout a
moveSubtree currentIndex fromMaybe currentId dist lay =
    case fromMaybe of
        Just from ->
            if from.index /= currentIndex - 1 then
                let
                    normDist =
                        dist / toFloat (currentIndex - from.index)
                in
                lay
                    |> updateTidyData
                        (\current ->
                            { current
                                | modifierToSubtree = current.modifierToSubtree + dist
                                , shiftAcceleration = current.shiftAcceleration - normDist
                                , shiftChange = current.shiftChange - (dist - normDist)
                            }
                        )
                        currentId
                    |> updateTidyData
                        (\from_ ->
                            { from_ | shiftAcceleration = from_.shiftAcceleration + normDist }
                        )
                        from.id

            else
                lay
                    |> updateTidyData
                        (\current ->
                            { current | modifierToSubtree = current.modifierToSubtree + dist }
                        )
                        currentId

        Nothing ->
            lay
                |> updateTidyData
                    (\current ->
                        { current | modifierToSubtree = current.modifierToSubtree + dist }
                    )
                    currentId


refToMaybe : Int -> TidyLayout a -> Maybe Int
refToMaybe ref lay =
    if ref >= 0 && ref < Array.length lay then
        Just ref

    else
        Nothing


contourNext lay ({ node, isLeft, modifierSum } as contour) =
    case node |> Maybe.andThen (derefTidyData lay) of
        Just current ->
            if isLeft then
                if Array.isEmpty current.children then
                    { contour | node = refToMaybe current.threadLeft lay, modifierSum = modifierSum + current.modifierThreadLeft }

                else
                    let
                        newNode =
                            Array.get 0 current.children

                        mod =
                            newNode |> Maybe.andThen (derefTidyData lay) |> Maybe.map (\c -> c.modifierToSubtree) |> Maybe.withDefault 0
                    in
                    { contour | node = newNode, modifierSum = modifierSum + mod }

            else if Array.isEmpty current.children then
                { contour | node = refToMaybe current.threadRight lay, modifierSum = modifierSum + current.modifierThreadRight }

            else
                let
                    newNode =
                        Array.get (Array.length current.children - 1) current.children

                    mod =
                        newNode |> Maybe.andThen (derefTidyData lay) |> Maybe.map (\c -> c.modifierToSubtree) |> Maybe.withDefault 0
                in
                { contour | node = newNode, modifierSum = modifierSum + mod }

        Nothing ->
            contour


setLeftThread modifier nodeId targetId currentIndex lay =
    case derefTidyData lay nodeId |> Maybe.map .children |> Maybe.andThen (\c -> Maybe.map2 Tuple.pair (Array.get 0 c |> Maybe.andThen (derefTidyData lay)) (Array.get currentIndex c |> Maybe.andThen (derefTidyData lay))) of
        Just ( first, current ) ->
            let
                diff =
                    modifier - first.modifierExtremeLeft - first.modifierToSubtree
            in
            lay
                |> updateTidyData
                    (\extremeLeft ->
                        { extremeLeft | threadLeft = targetId, modifierThreadLeft = diff }
                    )
                    first.extremeLeft
                |> updateTidyData
                    (\first_ ->
                        { first_ | extremeLeft = current.extremeLeft, modifierExtremeLeft = current.modifierExtremeLeft + current.modifierToSubtree - first_.modifierToSubtree }
                    )
                    first.id

        Nothing ->
            lay


setRightThread modifier nodeId targetId currentIndex lay =
    case derefTidyData lay nodeId |> Maybe.map .children |> Maybe.andThen (\c -> Maybe.map2 Tuple.pair (Array.get (currentIndex - 1) c |> Maybe.andThen (derefTidyData lay)) (Array.get currentIndex c |> Maybe.andThen (derefTidyData lay))) of
        Just ( prev, current ) ->
            let
                diff =
                    modifier - current.modifierExtremeRight - current.modifierToSubtree
            in
            lay
                |> updateTidyData
                    (\extremeRight ->
                        { extremeRight | threadRight = targetId, modifierThreadRight = diff }
                    )
                    current.extremeRight
                |> updateTidyData
                    (\current_ ->
                        { current_ | extremeRight = prev.extremeRight, modifierExtremeRight = prev.modifierExtremeRight + prev.modifierToSubtree - current_.modifierToSubtree }
                    )
                    current.id

        Nothing ->
            lay


separate : Float -> Int -> Int -> TidyLayout a -> YList -> ( TidyLayout a, YList )
separate peerMargin childIndex nodeId lay_ ylist_ =
    let
        nodeChildren =
            derefTidyData lay_ nodeId |> Maybe.map .children

        go leftContour rightContour lay yList =
            case ( leftContour.node, rightContour.node ) of
                ( Just left, Just right ) ->
                    let
                        yList2 =
                            if derefBottom left lay > (yList |> List.head |> Maybe.map .y |> Maybe.withDefault 0) then
                                List.tail yList |> Maybe.withDefault []

                            else
                                yList

                        dist =
                            (derefTidyData lay left |> Maybe.map (\{ relativeX, width } -> leftContour.modifierSum + relativeX + width / 2) |> Maybe.withDefault 0) - (derefTidyData lay right |> Maybe.map (\{ relativeX, width } -> rightContour.modifierSum + relativeX - width / 2) |> Maybe.withDefault 0) + peerMargin

                        ( rightContour1, lay1 ) =
                            if dist > 0 then
                                -- left and right are too close. move right part with distance of dist
                                ( { rightContour | modifierSum = rightContour.modifierSum + dist }
                                , moveSubtree childIndex (yList2 |> List.head) (nodeChildren |> Maybe.andThen (Array.get childIndex) |> Maybe.withDefault -1) dist lay
                                )

                            else
                                ( rightContour, lay )

                        leftBottom =
                            derefBottom left lay1

                        rightBottom =
                            derefBottom right lay1
                    in
                    if leftBottom < rightBottom then
                        go (contourNext lay1 leftContour) rightContour1 lay1 yList2

                    else if leftBottom > rightBottom then
                        go leftContour (contourNext lay1 rightContour1) lay1 yList2

                    else
                        go (contourNext lay1 leftContour) (contourNext lay1 rightContour1) lay1 yList2

                ( Nothing, Just right ) ->
                    ( setLeftThread rightContour.modifierSum nodeId right childIndex lay, yList )

                ( Just left, Nothing ) ->
                    ( setRightThread leftContour.modifierSum nodeId left childIndex lay, yList )

                ( Nothing, Nothing ) ->
                    ( lay, yList )

        newContour isLeft index =
            let
                node =
                    Maybe.andThen (Array.get index) nodeChildren
            in
            { isLeft = isLeft, modifierSum = node |> Maybe.andThen (derefTidyData lay_) |> Maybe.map .modifierToSubtree |> Maybe.withDefault 0, node = node }
    in
    go (newContour False (childIndex - 1)) (newContour True childIndex) lay_ ylist_


positionRoot : Int -> TidyLayout a -> TidyLayout a
positionRoot nodeId lay =
    case derefTidyData lay nodeId |> Maybe.map .children |> Maybe.andThen (\c -> Maybe.map2 Tuple.pair (Array.get 0 c |> Maybe.andThen (derefTidyData lay)) (Array.get (Array.length c - 1) c |> Maybe.andThen (derefTidyData lay))) of
        Just ( first, last ) ->
            let
                relativeX =
                    (first.relativeX + first.modifierToSubtree + last.relativeX + last.modifierToSubtree)
                        / 2
            in
            lay
                |> updateTidyData
                    (\self ->
                        { self | relativeX = relativeX, modifierToSubtree = -relativeX }
                    )
                    nodeId

        Nothing ->
            lay


addChildSpacing : Array Id -> TidyLayout a -> TidyLayout a
addChildSpacing children layout_ =
    Array.foldl
        (\idx ( lay, speed, delta ) ->
            let
                ( childShiftAcceleration, shiftChange ) =
                    derefTidyData lay idx
                        |> Maybe.map (\c -> ( c.shiftAcceleration, c.shiftChange ))
                        |> Maybe.withDefault ( 0, 0 )
            in
            ( updateTidyData
                (\child ->
                    { child
                        | modifierToSubtree = child.modifierToSubtree + speed + childShiftAcceleration + delta + shiftChange
                        , shiftAcceleration = 0
                        , shiftChange = 0
                    }
                )
                idx
                lay
            , speed + childShiftAcceleration
            , delta + speed + childShiftAcceleration + shiftChange
            )
        )
        ( layout_, 0, 0 )
        children
        |> (\( a, _, _ ) -> a)


layout :
    { nodeSize : a -> ( Float, Float )
    , layered : Bool
    , parentChildMargin : Float
    , peerMargin : Float
    }
    -> Tree a
    -> Tree { height : Float, node : a, width : Float, x : Float, y : Float }
layout getters tree =
    let
        defaultValue =
            Tree.label tree

        init =
            initialize getters.nodeSize

        setYRecursive =
            if getters.layered then
                traverseBFSWithDepth
                    (\depth depths tidy ->
                        case Dict.get (depth - 1) depths of
                            Just prevMax ->
                                ( Dict.insert depth (max (Dict.get depth depths |> Maybe.withDefault 0) (prevMax + tidy.height + getters.parentChildMargin)) depths, { tidy | y = prevMax } )

                            Nothing ->
                                ( Dict.insert depth (tidy.height + getters.parentChildMargin) depths, { tidy | y = 0 } )
                    )
                    Dict.empty

            else
                traverseWithParent
                    (\{ parent, node } ->
                        { node
                            | y =
                                parent
                                    |> Maybe.map (\parents -> parents.height + parents.y + getters.parentChildMargin)
                                    |> Maybe.withDefault 0
                        }
                    )

        runWalk fn lay =
            fn 0 lay

        firstWalk idx lay =
            case derefTidyData lay idx of
                Nothing ->
                    lay

                Just node ->
                    case Array.toList node.children of
                        [] ->
                            setExtreme idx lay

                        firstChildId :: rest ->
                            let
                                lay1 =
                                    firstWalk firstChildId lay
                            in
                            case derefTidyData lay1 firstChildId of
                                Nothing ->
                                    lay1

                                Just firstChildData ->
                                    let
                                        yListInitDatum =
                                            derefBottom firstChildData.extremeRight lay1

                                        yListInit =
                                            [ { y = yListInitDatum, index = 0, id = firstChildId } ]

                                        res =
                                            List.foldl
                                                (\id { yList, layN, index } ->
                                                    let
                                                        lay2 =
                                                            firstWalk id layN

                                                        maxY =
                                                            derefTidyData lay2 id
                                                                |> Maybe.map
                                                                    (\{ extremeLeft } ->
                                                                        derefBottom extremeLeft lay2
                                                                    )
                                                                |> Maybe.withDefault 0

                                                        ( lay3, yList1 ) =
                                                            separate getters.peerMargin index idx lay2 yList

                                                        update lst =
                                                            case lst of
                                                                [] ->
                                                                    [ { index = index, y = maxY, id = id } ]

                                                                head :: tail ->
                                                                    if head.y <= maxY then
                                                                        update tail

                                                                    else
                                                                        { index = index, y = maxY, id = id } :: tail
                                                    in
                                                    { yList = update yList1, layN = lay3, index = index + 1 }
                                                )
                                                { yList = yListInit, layN = lay1, index = 1 }
                                                rest
                                    in
                                    res.layN
                                        |> positionRoot idx
                                        |> setExtreme idx

        secondWalk modSum idx lay =
            case derefTidyData lay idx of
                Just { width, height, y, modifierToSubtree, relativeX, value, children } ->
                    Array.map (\a -> secondWalk (modSum + modifierToSubtree) a (addChildSpacing children lay)) children
                        |> Array.toList
                        |> Tree.tree { width = width, height = height, y = y, x = relativeX + modSum + modifierToSubtree - width / 2, node = value }

                Nothing ->
                    -- can't happen
                    Tree.singleton { width = 0 / 0, height = 0 / 0, x = 0 / 0, y = 0 / 0, node = defaultValue }
    in
    tree
        |> init
        |> setYRecursive
        |> runWalk firstWalk
        |> runWalk (secondWalk 0)

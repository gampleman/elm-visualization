module Hierarchy.Tidy exposing (..)

{-| Based on <https://github.com/zxch3n/tidy/blob/master/rust/crates/tidy-tree/src/layout/tidy_layout.rs>.

For a description, see <https://www.zxch3n.com/tidy/tidy/>.

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Hierarchy exposing (Hierarchy(..))



--- Debug


debugTree : String -> TidyLayout a -> TidyLayout a
debugTree prefix ((TidyLayout { tree }) as lay) =
    let
        go indent (Hierarchy node children) =
            case derefTidyData lay node.id of
                Just tidy ->
                    String.join "\n" ((String.repeat (indent * 2) " " ++ Debug.toString tidy) :: List.map (go (indent + 1)) children)

                Nothing ->
                    ""

        _ =
            Debug.log (prefix ++ ": \n" ++ go 0 tree) ()
    in
    lay


type TidyLayout a
    = TidyLayout
        { tree : Hierarchy Node
        , origingal : Hierarchy a
        , mutable : Dict Int TidyData
        }


type alias Node =
    { id : Int
    , width : Float
    , height : Float
    }


type alias TidyData =
    { threadLeft : Int
    , threadRight : Int
    , -- this.extreme_left == this.thread_left.extreme_left ||
      -- this.extreme_left == this.children[0].extreme_left
      extremeLeft : Int
    , -- this.extreme_right == this.thread_right.extreme_right ||
      -- this.extreme_right == this.children[-1].extreme_right
      extremeRight : Int
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
    , children : Array Int
    , id : Int
    }


traverseWithParent : ({ parent : Maybe { node : Node, tidy : TidyData }, node : Node, tidy : TidyData } -> TidyData) -> TidyLayout a -> TidyLayout a
traverseWithParent fn ((TidyLayout { tree }) as l) =
    let
        help maybeParent (Hierarchy val children) lay =
            let
                newL =
                    updateTidyData
                        (\tidy ->
                            fn
                                { parent =
                                    Maybe.andThen
                                        (\parent ->
                                            derefTidyData lay parent.id
                                                |> Maybe.map (\d -> { node = parent, tidy = d })
                                        )
                                        maybeParent
                                , node = val
                                , tidy = tidy
                                }
                        )
                        val.id
                        lay
            in
            List.foldl (help (Just val)) newL children
    in
    help Nothing tree l


traverseBFSWithDepth : (Int -> a -> Node -> TidyData -> ( a, TidyData )) -> a -> TidyLayout b -> TidyLayout b
traverseBFSWithDepth fn init (TidyLayout ({ tree, mutable } as lay)) =
    let
        collect depth (Hierarchy val children) =
            List.map (\(Hierarchy node _) -> ( depth, node )) children ++ List.concatMap (collect (depth + 1)) children

        initial =
            case tree of
                Hierarchy v _ ->
                    ( 0, v )

        ( newMutable, _ ) =
            List.foldl
                (\( depth, node ) ( l, accu ) ->
                    case Debug.log "processing" (Dict.get node.id l) of
                        Just td ->
                            let
                                ( newAccu, newL ) =
                                    fn depth (Debug.log "accu" accu) node td
                            in
                            ( Dict.insert node.id newL l, newAccu )

                        Nothing ->
                            ( l, accu )
                )
                ( mutable, init )
                (Debug.log "depth list" (initial :: collect 1 tree))
    in
    TidyLayout { lay | mutable = newMutable }


initialize : (a -> Float) -> (a -> Float) -> Hierarchy a -> TidyLayout a
initialize width height tree =
    let
        go idx (Hierarchy a c) dict =
            let
                ( newChildren, subDict, maxIndex ) =
                    List.foldr
                        (\item ( newList, accum, startIndex ) ->
                            let
                                ( newItem, newAccum, index ) =
                                    go startIndex item accum
                            in
                            ( newItem :: newList, newAccum, index )
                        )
                        ( [], dict, idx )
                        c

                width_ =
                    width a

                height_ =
                    height a
            in
            ( Hierarchy { id = maxIndex, width = width_, height = height_ } newChildren
            , Dict.insert
                maxIndex
                { threadLeft = -1
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
                , width = width_
                , height = height_
                , children = Array.fromList (List.map (\(Hierarchy { id } _) -> id) newChildren)
                , id = maxIndex
                }
                subDict
            , maxIndex + 1
            )

        ( newTree, mutable, _ ) =
            go 0 tree Dict.empty
    in
    TidyLayout { tree = newTree, mutable = mutable, origingal = tree }


derefTidyData : TidyLayout a -> Int -> Maybe TidyData
derefTidyData (TidyLayout l) id =
    Dict.get id l.mutable


derefTidyDataFromNode : Hierarchy Node -> TidyLayout a -> Maybe TidyData
derefTidyDataFromNode (Hierarchy { id } _) lay =
    derefTidyData lay id


updateTidyData : (TidyData -> TidyData) -> Int -> TidyLayout a -> TidyLayout a
updateTidyData fn id (TidyLayout l) =
    case Dict.get id l.mutable of
        Just td ->
            TidyLayout { l | mutable = Dict.insert id (fn td) l.mutable }

        Nothing ->
            TidyLayout l


derefBottom : Int -> TidyLayout a -> Float
derefBottom id l =
    derefTidyData l id
        |> Maybe.map (\{ height, y } -> height + y)
        |> Maybe.withDefault 0


setExtreme : Hierarchy Node -> TidyLayout a -> TidyLayout a
setExtreme (Hierarchy a children) tidyLayout =
    updateTidyData
        (\tidy ->
            case
                Maybe.map2 Tuple.pair
                    (children |> List.head |> Maybe.andThen (\n -> derefTidyDataFromNode n tidyLayout))
                    (children |> List.reverse |> List.head |> Maybe.andThen (\n -> derefTidyDataFromNode n tidyLayout))
            of
                Nothing ->
                    { tidy | extremeLeft = a.id, extremeRight = a.id, modifierExtremeLeft = 0, modifierExtremeRight = 0 }

                Just ( first, last ) ->
                    { tidy | extremeLeft = first.extremeLeft, extremeRight = last.extremeRight, modifierExtremeLeft = first.modifierToSubtree + first.modifierExtremeLeft, modifierExtremeRight = last.modifierToSubtree + last.modifierExtremeRight }
        )
        a.id
        tidyLayout



-- type alias Node a =
--     { id : Int
--     , width : Float
--     , height : Float
--     , x : Float
--     , y : Float
--     , -- node x position relative to its parent
--       relativeX : Float
--     , -- node y position relative to its parent
--       relativeY : Float
--     -- , bbox : { width : Float, height : Float }
--     , tidy : Maybe TidyData
--     , datum : a
--     }


type alias YList =
    List { index : Int, id : Int, y : Float }


moveSubtree : Int -> Maybe { y : Float, id : Int, index : Int } -> Int -> Float -> TidyLayout a -> TidyLayout a
moveSubtree currentIndex fromMaybe currentId dist lay =
    let
        _ =
            Debug.log "moveSubtree" ( ( currentId, currentIndex ), fromMaybe, dist )
    in
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
refToMaybe ref (TidyLayout { mutable }) =
    if Dict.member ref mutable then
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
            case ( (Debug.log "leftContour" leftContour).node, (Debug.log "rightContour" rightContour).node ) of
                ( Just left, Just right ) ->
                    let
                        yList2 =
                            if Debug.log "leftBottom" (derefBottom left lay) > Debug.log "yBottom" (yList |> List.head |> Maybe.map .y |> Maybe.withDefault 0) then
                                List.tail yList |> Maybe.withDefault []

                            else
                                yList

                        dist =
                            (derefTidyData lay left |> Maybe.map (\{ relativeX, width } -> leftContour.modifierSum + relativeX + width / 2) |> Maybe.withDefault 0) - (derefTidyData lay right |> Maybe.map (\{ relativeX, width } -> rightContour.modifierSum + relativeX - width / 2) |> Maybe.withDefault 0) + peerMargin

                        _ =
                            Debug.log "left.right, right.left, peerMargin"
                                ( derefTidyData lay left |> Maybe.map (\{ relativeX, width } -> leftContour.modifierSum + relativeX + width / 2) |> Maybe.withDefault 0
                                , derefTidyData lay right |> Maybe.map (\{ relativeX, width } -> rightContour.modifierSum + relativeX - width / 2) |> Maybe.withDefault 0
                                , peerMargin
                                )

                        ( rightContour1, lay1 ) =
                            if Debug.log "dist in go/separate" dist > 0 then
                                -- left and right are too close. move right part with distance of dist
                                ( { rightContour | modifierSum = rightContour.modifierSum + dist }
                                , moveSubtree childIndex (yList2 |> Debug.log "yList2" |> List.head) (nodeChildren |> Maybe.andThen (Array.get childIndex) |> Maybe.withDefault -1) dist lay
                                )

                            else
                                ( rightContour, lay )

                        leftBottom =
                            derefBottom left lay1

                        rightBottom =
                            derefBottom right lay1
                    in
                    if leftBottom < rightBottom then
                        let
                            _ =
                                Debug.log "Going left" ()
                        in
                        go (contourNext lay1 leftContour) rightContour1 lay1 yList2

                    else if leftBottom > rightBottom then
                        let
                            _ =
                                Debug.log "Going right" ()
                        in
                        go leftContour (contourNext lay1 rightContour1) lay1 yList2

                    else
                        let
                            _ =
                                Debug.log "Going both" ()
                        in
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
                _ =
                    Debug.log "(first, last)" ( first, last )

                relativeX =
                    (first.relativeX + first.modifierToSubtree + last.relativeX + last.modifierToSubtree)
                        / 2
                        |> Debug.log "positionRoot"
            in
            lay
                |> updateTidyData
                    (\self ->
                        { self | relativeX = relativeX, modifierToSubtree = -relativeX }
                    )
                    nodeId

        Nothing ->
            lay


addChildSpacing : List (Hierarchy Node) -> TidyLayout a -> TidyLayout a
addChildSpacing children layout_ =
    List.foldl
        (\(Hierarchy node _) ( lay, speed, delta ) ->
            let
                ( childShiftAcceleration, shiftChange ) =
                    derefTidyData lay node.id
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
                node.id
                lay
            , speed + childShiftAcceleration
            , delta + speed + childShiftAcceleration + shiftChange
            )
        )
        ( layout_, 0, 0 )
        children
        |> (\( a, _, _ ) -> a)


logLayoutProperty : String -> Int -> (TidyData -> a) -> TidyLayout b -> TidyLayout b
logLayoutProperty label id getter lay =
    let
        _ =
            case derefTidyData lay id of
                Just d ->
                    Debug.log label (getter d)

                Nothing ->
                    Debug.todo (label ++ "property not found")
    in
    lay


layout :
    { width : a -> Float, height : a -> Float, layered : Bool, parentChildMargin : Float, peerMargin : Float }
    -> Hierarchy a
    -> Hierarchy { height : Float, value : a, width : Float, x : Float, y : Float, relativeX : Float, modifierToSubtree : Float }
layout getters tree =
    let
        init =
            initialize getters.width getters.height

        setYRecursive =
            if getters.layered then
                traverseBFSWithDepth
                    (\depth depths _ tidy ->
                        case Dict.get (depth - 1) depths of
                            Just prevMax ->
                                ( Dict.insert depth (max (Dict.get depth depths |> Maybe.withDefault 0) (prevMax + tidy.height + getters.parentChildMargin)) depths, { tidy | y = prevMax } )

                            Nothing ->
                                ( Dict.insert depth (tidy.height + getters.parentChildMargin) depths, { tidy | y = 0 } )
                    )
                    Dict.empty

            else
                traverseWithParent
                    (\{ parent, tidy } ->
                        -- TODO: Deal with layered
                        { tidy
                            | y =
                                parent
                                    |> Maybe.map (\parents -> parents.node.height + parents.tidy.y + getters.parentChildMargin)
                                    |> Maybe.withDefault 0
                        }
                    )

        runWalk fn ((TidyLayout x) as lay) =
            fn x.tree lay

        firstWalk ((Hierarchy nodeVal children) as node) lay =
            case Debug.log "firstWalk" children of
                [] ->
                    setExtreme node lay

                ((Hierarchy firstChild _) as firstChildNode) :: rest ->
                    let
                        lay1 =
                            firstWalk firstChildNode lay

                        yListInitDatum =
                            derefTidyDataFromNode firstChildNode lay1
                                |> Maybe.map
                                    (\{ extremeRight } ->
                                        derefBottom (Debug.log "extremeRight" extremeRight) lay1
                                    )
                                |> Debug.log "yListInitDatum"
                                |> Maybe.withDefault 0

                        yListInit =
                            [ { y = yListInitDatum, index = 0, id = firstChild.id } ]

                        res =
                            List.foldl
                                (\((Hierarchy { id } _) as childNode) { yList, layN, index } ->
                                    let
                                        lay2 =
                                            firstWalk (Debug.log "running firstWalk loop" childNode) layN

                                        maxY =
                                            derefTidyDataFromNode childNode lay2
                                                |> Maybe.map
                                                    (\{ extremeLeft } ->
                                                        derefBottom extremeLeft lay2
                                                    )
                                                |> Maybe.withDefault 0

                                        ( lay3, yList1 ) =
                                            separate getters.peerMargin index nodeVal.id lay2 (Debug.log "input yList" yList)

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
                        |> positionRoot nodeVal.id
                        |> setExtreme node

        secondWalk modSum (Hierarchy val valChildren) ((Hierarchy nodeVal children) as node) lay =
            case derefTidyData lay nodeVal.id of
                Just { width, height, y, modifierToSubtree, relativeX } ->
                    List.map2 (\a b -> secondWalk (modSum + modifierToSubtree) a b (addChildSpacing children lay)) valChildren children
                        |> Hierarchy { width = width, height = height, y = y, x = relativeX + modSum + modifierToSubtree, value = val, relativeX = relativeX, modifierToSubtree = modifierToSubtree }

                Nothing ->
                    Debug.todo ""
    in
    tree
        |> init
        |> setYRecursive
        |> debugTree "setYRecursive"
        |> runWalk firstWalk
        |> debugTree "firstWalk"
        |> runWalk (secondWalk 0 tree)


links : Hierarchy { height : Float, value : a, width : Float, x : Float, y : Float, relativeX : Float, modifierToSubtree : Float } -> List ( { height : Float, value : a, width : Float, x : Float, y : Float, relativeX : Float, modifierToSubtree : Float }, { height : Float, value : a, width : Float, x : Float, y : Float, relativeX : Float, modifierToSubtree : Float } )
links (Hierarchy node children) =
    List.concatMap (\((Hierarchy child _) as childNode) -> ( node, child ) :: links childNode) children

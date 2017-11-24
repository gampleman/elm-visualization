module Visualization.QuadTree exposing (QuadTree, Pointed, empty, insert, expand, fromList, length, topLevelInsert)


type alias Pointed a =
    { a | x : Float, y : Float }


type alias BBox =
    { x0 : Float, y0 : Float, x1 : Float, y1 : Float }


type QuadTree agregate item
    = Empty
    | Leaf BBox (List item)
    | Node BBox agregate (QuadTree agregate item) (QuadTree agregate item) (QuadTree agregate item) (QuadTree agregate item)


empty : QuadTree agregate item
empty =
    Empty


length : QuadTree agregate item -> Int
length tree =
    case tree of
        Empty ->
            0

        Leaf _ list ->
            List.length list

        Node _ _ nw ne se sw ->
            length nw + length ne + length se + length sw



-- TODO: optimize this with precumputing extent


fromList : List (Pointed a) -> QuadTree () (Pointed a)
fromList =
    List.foldl topLevelInsert empty


center : BBox -> ( Float, Float )
center { x0, y0, x1, y1 } =
    ( (x0 + x1) / 2, (y0 + y1) / 2 )


quadrants : BBox -> ( BBox, BBox, BBox, BBox )
quadrants ({ x0, y0, x1, y1 } as bbox) =
    let
        ( xm, ym ) =
            center bbox
    in
        ( BBox x0 y0 xm ym, BBox xm y0 x1 ym, BBox xm ym x1 y1, BBox x0 ym xm y1 )


splitInsert : BBox -> Pointed item -> Pointed item -> List (Pointed item) -> QuadTree () (Pointed item)
splitInsert bbox item head tail =
    let
        ( nw, ne, se, sw ) =
            quadrants bbox

        quadrant q =
            -- let
            --     _ =
            --         Debug.log "quadrant" ( q, item, head, contains q item, contains q head )
            -- in
            case ( contains q item, contains q head ) of
                ( True, True ) ->
                    splitInsert q item head tail

                ( True, False ) ->
                    Leaf q [ item ]

                ( False, True ) ->
                    Leaf q (head :: tail)

                ( False, False ) ->
                    Empty
    in
        Node bbox () (quadrant nw) (quadrant ne) (quadrant se) (quadrant sw)


contains : BBox -> Pointed a -> Bool
contains { x0, y0, x1, y1 } { x, y } =
    (x0 <= x) && (x <= x1) && (y0 <= y) && (y <= y1)


topLevelInsert item tree =
    let
        _ =
            Debug.log "insert" ( item, tree )
    in
        insert item tree


insert : Pointed item -> QuadTree () (Pointed item) -> QuadTree () (Pointed item)
insert item tree =
    case tree of
        Empty ->
            Leaf (BBox (toFloat (floor item.x)) (toFloat (floor item.y)) (toFloat (floor item.x + 1)) (toFloat (floor item.y + 1))) [ item ]

        Leaf bbox (head :: tail) ->
            if (item.x == head.x) && (item.y == head.y) then
                Leaf bbox <| item :: (head :: tail)
            else
                splitInsert { x0 = min bbox.x0 item.x, y0 = min bbox.y0 item.y, x1 = max bbox.x1 item.x, y1 = max bbox.y1 item.y } item head tail

        Leaf bbox [] ->
            Leaf bbox [ item ]

        Node bbox _ nw ne se sw ->
            let
                inQuadrant q =
                    contains q item

                ( nwq, neq, seq, swq ) =
                    quadrants bbox
            in
                case ( inQuadrant nwq, inQuadrant neq, inQuadrant seq, inQuadrant swq ) of
                    ( True, _, _, _ ) ->
                        Node bbox () (insert item nw) ne se sw

                    ( _, True, _, _ ) ->
                        Node bbox () nw (insert item ne) se sw

                    ( _, _, True, _ ) ->
                        Node bbox () nw ne (insert item se) sw

                    ( _, _, _, True ) ->
                        Node bbox () nw ne se (insert item sw)

                    _ ->
                        insert item <| expand item tree


expand : Pointed a -> QuadTree b c -> QuadTree b c
expand { x, y } tree =
    case tree of
        Node { x0, x1, y0, y1 } aggregate _ _ _ _ ->
            if x0 > x || x > x1 || y0 > y || y > y1 then
                let
                    -- _ =
                    --     Debug.log "performing expand" ""
                    left =
                        x < ((x0 + x1) / 2)

                    top =
                        y < ((y0 + y1) / 2)

                    width =
                        x1 - x0
                in
                    expand { x = x, y = y } <|
                        case ( left, top ) of
                            ( True, True ) ->
                                Node { x0 = x0 - width, y0 = y0 - width, x1 = x1, y1 = y1 } aggregate Empty Empty tree Empty

                            ( True, False ) ->
                                Node { x0 = x0 - width, y0 = y0, x1 = x1, y1 = y1 + width } aggregate Empty tree Empty Empty

                            ( False, True ) ->
                                Node { x0 = x0, y0 = y0 - width, x1 = x1 + width, y1 = y1 } aggregate Empty Empty Empty tree

                            ( False, False ) ->
                                Node { x0 = x0, y0 = y0, x1 = x1 + width, y1 = y1 + width } aggregate tree Empty Empty Empty
            else
                tree

        otherwise ->
            tree

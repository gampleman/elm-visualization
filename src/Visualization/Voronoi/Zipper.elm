module Visualization.Voronoi.Zipper exposing (..)


type Zipper a
    = Zipper (List a) a (List a)


singleton x =
    Zipper [] x []


toList (Zipper q x n) =
    (x :: n) ++ (List.reverse q)


forward ((Zipper q x n) as z) =
    case n of
        [] ->
            case List.reverse q of
                [] ->
                    Zipper q x n

                y :: ys ->
                    Zipper [] y (ys ++ [ x ])

        y :: ys ->
            Zipper (x :: q) y ys


backward ((Zipper q x n) as z) =
    case q of
        [] ->
            case List.reverse n of
                [] ->
                    Zipper q x n

                y :: ys ->
                    Zipper (ys ++ [ x ]) y []

        y :: ys ->
            Zipper ys y (x :: n)


next ((Zipper q x n) as z) =
    case n of
        [] ->
            case List.reverse q of
                [] ->
                    x

                y :: _ ->
                    y

        y :: _ ->
            y


current (Zipper _ x _) =
    x


previous ((Zipper q x n) as z) =
    case q of
        [] ->
            case List.reverse n of
                [] ->
                    x

                y :: _ ->
                    y

        y :: _ ->
            y


insert : a -> Zipper a -> Zipper a
insert item (Zipper q x n) =
    Zipper (x :: q) item n


replace : a -> Zipper a -> Zipper a
replace item =
    mapCurrent (always item)


mapCurrent : (a -> a) -> Zipper a -> Zipper a
mapCurrent fun (Zipper q x n) =
    Zipper q (fun x) n


remove : Zipper a -> Zipper a
remove (Zipper q x n) =
    case n of
        [] ->
            case List.reverse q of
                [] ->
                    Zipper q x n

                y :: ys ->
                    Zipper [] y ys

        y :: ys ->
            Zipper q y ys


findBy : (a -> Bool) -> Zipper a -> Zipper a
findBy f zipper =
    -- let
    --     start =
    --         (current zipper)
    --
    --     helper zipper =
    --         if Debug.log "current" (current zipper) == Debug.log "start" start then
    --             Debug.crash "Not found"
    --         else if current zipper == item then
    --             zipper
    --         else
    --             helper (forward zipper)
    -- in
    if f (current zipper) then
        zipper
    else
        findBy f (forward zipper)


walkForward : (Zipper a -> Bool) -> Zipper a -> Zipper a
walkForward cond zipper =
    if cond zipper then
        walkForward cond (forward zipper)
    else
        zipper

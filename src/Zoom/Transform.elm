module Zoom.Transform exposing (Transform, identity, invert, scale, toString, translate)


type alias Transform =
    { k : Float, x : Float, y : Float }


identity : Transform
identity =
    Transform 1.0 0.0 0.0


scale : Float -> Transform -> Transform
scale k_ { k, x, y } =
    Transform (k * k_) x y


invert : ( Float, Float ) -> Transform -> ( Float, Float )
invert ( locX, locY ) { k, x, y } =
    ( (locX - x) / k, (locY - y) / k )


toString : Transform -> String
toString { k, x, y } =
    "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ") scale(" ++ String.fromFloat k ++ ")"


translate : ( Float, Float ) -> Transform -> Transform
translate ( locX, locY ) { k, x, y } =
    Transform k (x + k * locX) (y + k * locY)

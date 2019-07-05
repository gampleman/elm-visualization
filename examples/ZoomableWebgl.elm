module ZoomableWebgl exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Json.Decode exposing (Value)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Random
import WebGL exposing (Mesh, Shader)
import Zoom exposing (Zoom)


w : Float
w =
    990


h : Float
h =
    504


main =
    Browser.element
        { init = \() -> ( Zoom.init { width = w, height = h }, Cmd.none )
        , view = view
        , update = \msg model -> ( Zoom.update msg model, Cmd.none )
        , subscriptions = \model -> Zoom.subscriptions model identity
        }


view zoom =
    WebGL.toHtml
        ([ width (round (w * 2))
         , height (round (h * 2))
         , style "display" "block"
         , style "width" (String.fromFloat w ++ "px")
         , style "height" (String.fromFloat h ++ "px")
         ]
            ++ Zoom.events zoom identity
        )
        [ WebGL.entity
            vertexShader
            fragmentShader
            mesh
            { zoom = zoomToMat4 zoom }
        ]


zoomToMat4 : Zoom -> Mat4
zoomToMat4 zoom =
    let
        z =
            Zoom.asRecord zoom
    in
    Mat4.makeTranslate3 ((z.translate.x + (w / 2) * z.scale) / (w / 2) - 1) (-(z.translate.y + (h / 2) * z.scale) / (h / 2) + 1) 0
        |> Mat4.scale3 z.scale z.scale 1


type alias Vertex =
    { position : Vec2
    }


n : Int
n =
    2 ^ 19


mesh : Mesh Vertex
mesh =
    Random.map2 (\x y -> { position = vec2 x y }) (Random.float -1 1) (Random.float -1 1)
        |> Random.list n
        |> Random.map WebGL.points
        |> (\gen -> Random.step gen (Random.initialSeed 7453723))
        -- chosen by fair dice role
        |> Tuple.first


type alias Uniforms =
    { zoom : Mat4 }


vertexShader : Shader Vertex Uniforms {}
vertexShader =
    [glsl|
        precision highp float;

        const float PI = 3.14159265359;

        const float a = -1.6;
        const float b = -2.0;
        const float c = -1.2;
        const float d = 2.0;

        uniform mat4 zoom;

        attribute vec2 position;

        void main() {
          float x1, x2 = position.x;
          float y1, y2 = position.y;
          for (int i = 0; i < 8; i++) {
            x1 = x2, y1 = y2;
            x2 = sin(a * y1) - cos(b * x1);
            y2 = sin(c * x1) - cos(d * y1);
          }
          gl_Position = zoom * vec4(x2 / 2.0, y2 / 2.0, 0.0, 1.0);
          gl_PointSize = 1.0 + smoothstep(1.0, 5.0, zoom[0][0]) * 3.0;
        }
    |]


fragmentShader : Shader {} Uniforms {}
fragmentShader =
    [glsl|
        precision mediump float;
        void main () {
            gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
        }
    |]

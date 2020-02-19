module Mandelbrot exposing (main)

{-| Try scrolling, double clicking, or pinch-and-zooming!

This example demonstrates how one can use the Zoom module for other rendering technologies, in this case for a WebGL scene.

-}

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
        { init =
            \() ->
                ( Zoom.init { width = w, height = h }
                    |> Zoom.translateExtent ( ( 0, 0 ), ( w, h ) )
                    |> Zoom.scaleExtent 1 10000
                , Cmd.none
                )
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
    WebGL.triangles
        [ ( Vertex (vec2 0 0)
          , Vertex (vec2 1 0)
          , Vertex (vec2 0 1)
          )
        , ( Vertex (vec2 0 1)
          , Vertex (vec2 1 0)
          , Vertex (vec2 1 1)
          )
        ]


type alias Uniforms =
    { zoom : Mat4 }


vertexShader : Shader Vertex Uniforms { v_pos : Vec2 }
vertexShader =
    [glsl|
        precision highp float;
        attribute vec2 position;
        uniform mat4 zoom;
        varying vec2 v_pos;

        void main() {
          v_pos  = vec2(position.x * 2.0 - 1.75 , position.y * 2.0 - 1.0);
          gl_Position = zoom * vec4(position * 2.0 - 1.0, 0, 1);
        }
    |]


fragmentShader : Shader {} Uniforms { v_pos : Vec2 }
fragmentShader =
    [glsl|
        precision highp float;
        varying vec2 v_pos;
        const float PI = 3.14159265359;

        vec4 sinebow(float iterations) {
            float t = 0.5 - (iterations / 100.0);
            return vec4(pow(sin(PI * t), 2.0), pow(sin(PI * (t + 1.0/ 3.0)), 2.0), pow(sin(PI * (t + 2.0/ 3.0)), 2.0), 1.0);
        }

        void main() {
          vec2 q = v_pos;
          vec4 color = vec4(0,0,0,1);
          vec2 z = vec2(0.0, 0.0);
          float iterations = 0.0;
          for(int i = 0; i < 100; i++) {
            iterations++;
            z = vec2(z.x * z.x - z.y * z.y, 2.0 * z.x * z.y) + q;
            if (length(z) > 10.0) break;
          }
          if (iterations < 100.0) {
              float log_zn = log(z.x*z.x + z.y*z.y) / 2.0;
              float nu = log(log_zn / log(2.0)) / log(2.0);
              iterations = iterations + 1.0 - nu;
              vec4 color1 = sinebow(floor(iterations));
              vec4 color2 = sinebow(floor(iterations) + 1.0);
              color = mix(color1, color2, mod(iterations, 1.0));
          } else {
            color = vec4(0,0,0,1);
          }
          gl_FragColor = color;
        }
    |]

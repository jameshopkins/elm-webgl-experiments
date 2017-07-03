module Court exposing (..)

import WebGL exposing (Mesh, Shader, Entity)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)
import WebGL.Texture as Texture exposing (Error, Texture)


type alias Uniforms =
    { perspective : Mat4
    , texture : Texture
    }


type alias Vertex =
    { position : Vec3
    , coord : Vec2
    }


court : Mesh Vertex
court =
    let
        -- This is crudely calculated based on the computed dimensions of the texture we load.
        -- This will ultimately be a fixed ratio based on the NBA court size.
        courtRatio =
            716 / 1261

        nearLeft =
            Vertex (vec3 -1 -1 (negate courtRatio)) (vec2 0 1)

        nearRight =
            Vertex (vec3 1 -1 (negate courtRatio)) (vec2 1 1)

        farLeft =
            Vertex (vec3 -1 -1 courtRatio) (vec2 0 0)

        farRight =
            Vertex (vec3 1 -1 courtRatio) (vec2 1 0)
    in
        [ ( nearLeft, nearRight, farLeft ), ( farLeft, farRight, nearRight ) ]
            |> WebGL.triangles


vertexShader : Shader Vertex Uniforms { vcoord : Vec2 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec2 coord;
        uniform mat4 perspective;
        varying vec2 vcoord;

        void main () {
            gl_Position = perspective * vec4(position, 1.0);
            vcoord = coord;
        }
    |]


fragmentShader : Shader {} { u | texture : Texture } { vcoord : Vec2 }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform sampler2D texture;
        varying vec2 vcoord;

        void main () {
            gl_FragColor = texture2D(texture, vcoord);
        }
    |]

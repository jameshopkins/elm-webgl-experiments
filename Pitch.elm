module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (width, height, style)
import WebGL exposing (Mesh, Shader)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Task
import WebGL.Texture as Texture exposing (Error, Texture)


type alias Model =
    { texture : Maybe Texture }


type Msg
    = TextureLoaded (Result Error Texture)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = (\_ -> Sub.none)
        , update = update
        }


init : ( Model, Cmd Msg )
init =
    ( Model Nothing, Task.attempt TextureLoaded (Texture.load "court.png") )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextureLoaded texture ->
            ( { model | texture = Result.toMaybe texture }, Cmd.none )


view : Model -> Html Msg
view { texture } =
    WebGL.toHtml
        [ width 400
        , height 400
        , style [ ( "display", "block" ) ]
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            pitch
            (Uniforms perspective texture)
        ]


type alias Uniforms =
    { perspective : Mat4
    , texture : Maybe Texture
    }


type alias Vertex =
    { position : Vec3
    , coord : Vec2
    }


perspective : Mat4
perspective =
    Mat4.mul
        (Mat4.makePerspective 45 1 0.01 100)
        (Mat4.makeLookAt (vec3 0 0 4) (vec3 0 0 0) (vec3 0 1 0))


pitch : Mesh Vertex
pitch =
    let
        nearLeft =
            Vertex (vec3 -1 -1 -1)

        nearRight =
            Vertex (vec3 1 -1 -1)

        farLeft =
            Vertex (vec3 -1 -1 1)

        farRight =
            Vertex (vec3 1 -1 1)
    in
        [ ( nearLeft, nearRight, farLeft ), ( farLeft, farRight, nearRight ) ]
            |> WebGL.triangles


vertexShader : Shader Vertex Uniforms {}
vertexShader =
    [glsl|
        attribute vec3 position;
        uniform mat4 perspective;

        void main () {
            gl_Position = perspective * vec4(position, 1.0);
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

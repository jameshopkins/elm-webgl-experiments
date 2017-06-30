module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (width, height, style)
import WebGL exposing (Mesh, Shader, Entity)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Task
import WebGL.Texture as Texture exposing (clampToEdge, Error, Texture)


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
    let
        loadTexture =
            Texture.load "crate.jpg"
    in
        ( Model Nothing, Task.attempt TextureLoaded loadTexture )


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
        (texture
            |> Maybe.map (scene perspective)
            |> Maybe.withDefault []
        )


scene : Mat4 -> Texture -> List Entity
scene camera texture =
    [ WebGL.entity
        vertexShader
        fragmentShader
        pitch
        { texture = texture, perspective = camera }
    ]


type alias Uniforms =
    { perspective : Mat4
    , texture : Texture
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
            Vertex (vec3 -1 -1 -1) (vec2 0 1)

        nearRight =
            Vertex (vec3 1 -1 -1) (vec2 1 1)

        farLeft =
            Vertex (vec3 -1 -1 1) (vec2 0 0)

        farRight =
            Vertex (vec3 1 -1 1) (vec2 1 0)
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

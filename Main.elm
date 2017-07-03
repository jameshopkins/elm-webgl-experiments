module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput)
import Html.Attributes exposing (width, height, min, max, style, type_, value)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Task
import WebGL exposing (Mesh, Shader, Entity)
import WebGL.Texture as Texture exposing (Error, Texture)
import Court


type alias Model =
    { texture : Maybe Texture
    , rotation : Float
    , zoom : Float
    }


type Msg
    = TextureLoaded (Result Error Texture)
    | RotateFloor String
    | Zoom String


scene : Mat4 -> Texture -> List Entity
scene perspective texture =
    [ WebGL.entity
        Court.vertexShader
        Court.fragmentShader
        Court.court
        { texture = texture, perspective = perspective }
    ]


perspective : Float -> Float -> Mat4
perspective angle zoom =
    let
        foo =
            zoom
    in
        List.foldr Mat4.mul
            Mat4.identity
            [ Mat4.makePerspective 45 1 0.01 100
            , Mat4.makeLookAt (vec3 0 foo foo) (vec3 0 0 0) (vec3 0 1 0)
            , Mat4.makeRotate (angle / 230) (vec3 0 1 0)
            ]


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
            -- This is pants. The image size is incorrect - yielding a SizeError
            Texture.loadWith Texture.nonPowerOfTwoOptions "court.jpg"
    in
        ( Model Nothing 0 2, Task.attempt TextureLoaded loadTexture )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RotateFloor deg ->
            ( { model | rotation = Result.withDefault 0 (String.toFloat deg) }, Cmd.none )

        Zoom l ->
            ( { model | zoom = Result.withDefault 0 (String.toFloat l) }, Cmd.none )

        TextureLoaded texture ->
            ( { model | texture = Result.toMaybe texture }, Cmd.none )


view : Model -> Html Msg
view { rotation, texture, zoom } =
    div []
        [ WebGL.toHtml
            [ width 800
            , height 800
            , style [ ( "display", "block" ) ]
            ]
            (texture
                |> Maybe.map (scene (perspective rotation zoom))
                |> Maybe.withDefault []
            )
        , input
            [ Html.Attributes.min "0"
            , Html.Attributes.max "360"
            , onInput RotateFloor
            , type_ "range"
            , value <| toString <| rotation
            ]
            []
        , input
            [ Html.Attributes.min "0.5"
            , Html.Attributes.max "4"
            , Html.Attributes.step "0.01"
            , onInput Zoom
            , type_ "range"
            , value <| toString <| zoom
            ]
            []
        ]

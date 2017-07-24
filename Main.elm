module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (on, onInput)
import Html.Attributes exposing (width, height, min, max, style, type_, value)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (getX, getY, vec2, Vec2)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Task
import WebGL exposing (Mesh, Shader, Entity)
import WebGL.Texture as Texture exposing (Error, Texture)
import Court
import Json.Decode as Json


type alias Model =
    { texture : Maybe Texture
    , rotation : Float
    , zoom : Float
    , screenCoords : Vec2
    }


type Msg
    = TextureLoaded (Result Error Texture)
    | RotateFloor String
    | Zoom String
    | SetScreenCoordinates Vec2


screenCoordinateOffsets : Json.Decoder Vec2
screenCoordinateOffsets =
    Json.map2 vec2
        (Json.field "offsetX" Json.float)
        (Json.field "offsetY" Json.float)


onClick : (Vec2 -> msg) -> Attribute msg
onClick tagger =
    on "click" (Json.map tagger screenCoordinateOffsets)


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
    List.foldr Mat4.mul
        Mat4.identity
        [ Mat4.makePerspective 45 1 0.01 100
        , Mat4.makeLookAt (vec3 0 zoom zoom) (vec3 0 0 0) (vec3 0 1 0)
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
        ( Model Nothing 0 2 (vec2 0 0), Task.attempt TextureLoaded loadTexture )


toDeviceCoordinates : Vec2 -> Vec2
toDeviceCoordinates coords =
    let
        x =
            (2 * (getX coords)) / 800 - 1

        y =
            1 - (2 * (getY coords)) / 800
    in
        vec2 x y


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RotateFloor deg ->
            ( { model | rotation = Result.withDefault 0 (String.toFloat deg) }, Cmd.none )

        Zoom l ->
            ( { model | zoom = Result.withDefault 0 (String.toFloat l) }, Cmd.none )

        TextureLoaded texture ->
            ( { model | texture = Result.toMaybe texture }, Cmd.none )

        SetScreenCoordinates coords ->
            ( { model | screenCoords = coords |> toDeviceCoordinates }, Cmd.none )


view : Model -> Html Msg
view { rotation, texture, zoom } =
    div []
        [ WebGL.toHtml
            [ width 800
            , height 800
            , style [ ( "display", "block" ) ]
            , onClick SetScreenCoordinates
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

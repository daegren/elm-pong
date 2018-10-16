module Main exposing (main)

import Browser
import Browser.Events
import Color
import Html exposing (Html)
import Svg
import Svg.Attributes



-- MODEL --


type alias Object a =
    { a
        | x : Float
        , y : Float
        , vx : Float
        , vy : Float
    }


type alias Ball =
    Object {}


type alias Model =
    {}


getDimensions : ( Int, Int )
getDimensions =
    ( 600, 400 )


getHalfWidths : ( Int, Int )
getHalfWidths =
    let
        ( w, h ) =
            getDimensions
    in
    ( w // 2, h // 2 )


initialModel : Model
initialModel =
    {}



-- INIT --


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



-- UPDATE --


type Msg
    = Tick Float


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    -- FIXME: acutally write the logic for update
    ( model, Cmd.none )



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ delta model ]


delta : Model -> Sub Msg
delta model =
    Browser.Events.onAnimationFrameDelta Tick



-- VIEW --


pongGreen : Color.Color
pongGreen =
    Color.rgb255 60 100 60


backgroundView =
    let
        ( gameWidth, gameHeight ) =
            getDimensions
    in
    Svg.rect
        [ Svg.Attributes.fill <| Color.toCssString pongGreen
        , Svg.Attributes.width (String.fromInt gameWidth)
        , Svg.Attributes.height (String.fromInt gameHeight)
        ]
        []


display : Model -> Svg.Svg msg
display model =
    let
        ( gameWidth, gameHeight ) =
            getDimensions
    in
    Svg.svg
        [ Svg.Attributes.width (String.fromInt gameWidth)
        , Svg.Attributes.height (String.fromInt gameHeight)
        ]
        [ backgroundView ]


body : Model -> List (Html.Html msg)
body model =
    [ display model ]


view : Model -> Browser.Document msg
view model =
    { title = "Elm Pong"
    , body = body model
    }



-- MAIN --


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

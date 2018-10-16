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


type alias Game =
    { ball : Ball }


defaultGame : Game
defaultGame =
    { ball = { x = 0, y = 0, vx = 200, vy = 200 } }


type alias Model =
    { gameState : Game }


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
    { gameState = defaultGame }



-- INIT --


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



-- UPDATE --


type Msg
    = Tick Float


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Tick delta ->
            ( { model
                | gameState = stepGame (delta / 1000) model.gameState
              }
            , Cmd.none
            )


stepGame : Float -> Game -> Game
stepGame delta game =
    let
        { ball } =
            game

        ball_ =
            stepBall delta ball
    in
    { game | ball = ball_ }


stepV : Float -> Bool -> Bool -> Float
stepV v lowerCollision upperCollision =
    if lowerCollision then
        abs v

    else if upperCollision then
        -(abs v)

    else
        v


stepBall : Float -> Ball -> Ball
stepBall delta ({ x, y, vx, vy } as ball) =
    let
        ( width, height ) =
            getDimensions
    in
    stepObj delta
        { ball
            | vx = stepV vx (x < 7) (x > toFloat width - 7)
            , vy = stepV vy (y < 7) (y > toFloat height - 7)
        }


stepObj : Float -> Object a -> Object a
stepObj delta ({ x, y, vx, vy } as obj) =
    { obj
        | x = x + vx * delta
        , y = y + vy * delta
    }



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrame model ]


onAnimationFrame : Model -> Sub Msg
onAnimationFrame model =
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
        [ backgroundView
        , displayBall model.gameState.ball
        ]


displayBall : Ball -> Svg.Svg msg
displayBall { x, y } =
    Svg.circle
        [ Svg.Attributes.cx (String.fromFloat x)
        , Svg.Attributes.cy (String.fromFloat y)
        , Svg.Attributes.r "15"
        , Svg.Attributes.fill <| Color.toCssString Color.white
        ]
        []


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

module Game exposing (Model, Msg, initialModel, subscriptions, update, view)

import Browser.Events
import Color
import Object exposing (Object)
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- MODEL --


type alias Ball =
    Object {}


type alias Model =
    { ball : Ball
    , dimensions : ( Float, Float )
    }


initialModel : ( Float, Float ) -> Model
initialModel dimensions =
    { ball = { x = 0, y = 0, vx = 200, vy = 200 }
    , dimensions = dimensions
    }



-- UPDATE --


type Msg
    = Tick Float


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick delta ->
            stepGame (delta / 1000) model


stepGame : Float -> Model -> Model
stepGame delta ({ ball } as model) =
    let
        ball_ =
            stepBall delta model ball
    in
    { model | ball = ball_ }


stepBall : Float -> Model -> Ball -> Ball
stepBall delta { dimensions } ({ x, y, vx, vy } as ball) =
    let
        ( width, height ) =
            dimensions
    in
    Object.step delta
        { ball
            | vx = Object.stepV vx (x < 7) (x > width - 7)
            , vy = Object.stepV vy (y < 7) (y > height - 7)
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


view : Model -> Svg msg
view ({ dimensions } as model) =
    let
        ( gameWidth, gameHeight ) =
            dimensions
    in
    svg
        [ width (String.fromFloat gameWidth)
        , height (String.fromFloat gameHeight)
        ]
        [ backgroundView model
        , displayBall model.ball
        ]


pongGreen : Color.Color
pongGreen =
    Color.rgb255 60 100 60


backgroundView { dimensions } =
    let
        ( gameWidth, gameHeight ) =
            dimensions
    in
    rect
        [ fill <| Color.toCssString pongGreen
        , width (String.fromFloat gameWidth)
        , height (String.fromFloat gameHeight)
        ]
        []


displayBall : Ball -> Svg msg
displayBall { x, y } =
    circle
        [ cx (String.fromInt (floor x))
        , cy (String.fromInt (floor y))
        , r "15"
        , fill <| Color.toCssString Color.white
        ]
        []

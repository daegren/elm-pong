module Game exposing (Model, Msg, initialModel, subscriptions, update, view)

import Browser.Events
import Color
import Object exposing (Object)
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- MODEL --


type alias Ball =
    Object {}


type alias Player =
    Object { score : Int }


player : Float -> Player
player x =
    { x = x, y = 0, vx = 0, vy = 0, score = 0 }


type alias Model =
    { ball : Ball
    , player1 : Player
    , dimensions : ( Float, Float )
    }


initialModel : ( Float, Float ) -> Model
initialModel (( width, height ) as dimensions) =
    { ball = { x = 0, y = 0, vx = 200, vy = 200 }
    , player1 = player 20
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
        , displayPlayer model.player1
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


displayPlayer : Player -> Svg msg
displayPlayer playerObj =
    rect
        [ fill <| Color.toCssString Color.white
        , width "10"
        , height "40"
        , translate playerObj
        ]
        []


translate : { r | x : Float, y : Float } -> Svg.Attribute msg
translate { x, y } =
    transform ("translate(" ++ String.fromFloat x ++ " " ++ String.fromFloat y ++ ")")

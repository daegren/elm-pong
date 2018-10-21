module Game exposing (Model, Msg, initialModel, subscriptions, update, view)

import Browser.Events
import Color
import Input
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


type State
    = Play
    | Pause


type alias Model =
    { state : State
    , ball : Ball
    , player1 : Player
    , player2 : Player
    , dimensions : ( Float, Float )
    }


initialModel : ( Float, Float ) -> Model
initialModel (( width, height ) as dimensions) =
    { state = Pause
    , ball = { x = width / 2, y = height / 2, vx = 200, vy = 200 }
    , player1 = player 20
    , player2 = player (width - 20)
    , dimensions = dimensions
    }



-- UPDATE --


type Msg
    = Tick Float


update : Msg -> Input.Model -> Model -> Model
update msg input model =
    case msg of
        Tick delta ->
            stepGame (delta / 1000) input model


stepGame : Float -> Input.Model -> Model -> Model
stepGame delta input ({ state, ball, player1, player2 } as model) =
    let
        ( score1, score2 ) =
            calculateScores model

        state_ =
            if input.space then
                Play

            else if score1 /= score2 then
                Pause

            else
                state

        ball_ =
            if state == Pause then
                ball

            else
                stepBall delta model ball

        player1_ =
            stepPlayer delta model input.paddle1 score1 player1

        player2_ =
            stepPlayer delta model input.paddle2 score2 player2
    in
    { model
        | state = state_
        , ball = ball_
        , player1 = player1_
        , player2 = player2_
    }


calculateScores : Model -> ( Int, Int )
calculateScores { dimensions, ball } =
    let
        ( width, height ) =
            dimensions

        score1 =
            if ball.x > width then
                1

            else
                0

        score2 =
            if ball.x < 0 then
                1

            else
                0
    in
    ( score1, score2 )


stepPlayer : Float -> Model -> Int -> Int -> Player -> Player
stepPlayer delta { dimensions } dir points playerObj =
    let
        ( width, height ) =
            dimensions

        player_ =
            Object.step delta { playerObj | vy = toFloat dir * 200 }

        y_ =
            clamp 0 (height - 40) player_.y

        score_ =
            playerObj.score + points
    in
    { player_ | y = y_, score = score_ }


stepBall : Float -> Model -> Ball -> Ball
stepBall delta { dimensions, player1, player2 } ({ x, y, vx, vy } as ball) =
    let
        ( width, height ) =
            dimensions
    in
    if x < 0 || not (near 0 width x) then
        { ball | x = width / 2, y = height / 2 }

    else
        Object.step delta
            { ball
                | vx = Object.stepV vx (within ball player1) (within ball player2)
                , vy = Object.stepV vy (y < 7) (y > height - 7)
            }



-- COLLISION HELPERS --


{-| Are n and m near each other?

Specifically are they within c of each other

-}
near : Float -> Float -> Float -> Bool
near n c m =
    m >= n - c && m <= n + c


within : Ball -> Player -> Bool
within ball playerObj =
    near playerObj.x 10 ball.x
        && near playerObj.y 40 ball.y



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    onAnimationFrame model


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
        , displayPlayer model.player2
        , displayHelp model
        , displayScores model
        ]


pongGreen : Color.Color
pongGreen =
    Color.rgb255 60 100 60


textGreen : Color.Color
textGreen =
    Color.rgb255 160 200 160


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
        , r "7"
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


displayHelp : Model -> Svg msg
displayHelp model =
    if model.state == Play then
        rect [] []

    else
        let
            ( width, height ) =
                model.dimensions
        in
        text_
            [ fontFamily "monospace"
            , fill <| Color.toCssString textGreen
            , translate { x = width / 2 - 125, y = 40 }
            ]
            [ text helpMessage ]


helpMessage : String
helpMessage =
    "SPACE to start ws and ⬆⬇ to move"


displayScores : Model -> Svg msg
displayScores { player1, player2, dimensions } =
    let
        ( width, height ) =
            dimensions
    in
    text_
        [ fontFamily "monospace"
        , fill <| Color.toCssString Color.white
        , fontSize "50"
        , translate { x = width / 2 - 50, y = height - 40 }
        ]
        [ text <|
            (String.fromInt player1.score ++ " " ++ String.fromInt player2.score)
        ]



-- VIEW HELPERS --


translate : { r | x : Float, y : Float } -> Svg.Attribute msg
translate { x, y } =
    transform
        ("translate("
            ++ String.fromInt (floor x)
            ++ " "
            ++ String.fromInt (floor y)
            ++ ")"
        )

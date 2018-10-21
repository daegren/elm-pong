module Main exposing (main)

import Browser
import Browser.Events
import Color
import Game
import Html exposing (Html)
import Input
import Svg
import Svg.Attributes



-- MODEL --


type alias Model =
    { gameState : Game.Model
    , input : Input.Model
    }


getDimensions : ( Float, Float )
getDimensions =
    ( 600, 400 )


initialModel : Model
initialModel =
    { gameState = Game.initialModel getDimensions
    , input = Input.initialModel
    }



-- INIT --


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



-- UPDATE --


type Msg
    = GameMsg Game.Msg
    | InputMsg Input.Msg


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        GameMsg subMsg ->
            let
                gameModel =
                    Game.update subMsg model.input model.gameState
            in
            ( { model | gameState = gameModel }, Cmd.none )

        InputMsg subMsg ->
            let
                inputModel =
                    Input.update subMsg model.input
            in
            ( { model | input = inputModel }, Cmd.none )



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map GameMsg <|
            Game.subscriptions model.gameState
        , Sub.map InputMsg <|
            Input.subscriptions
        ]



-- VIEW --


body : Model -> List (Html.Html msg)
body model =
    [ Game.view model.gameState ]


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

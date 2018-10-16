module Main exposing (main)

import Browser
import Browser.Events
import Color
import Game
import Html exposing (Html)
import Svg
import Svg.Attributes



-- MODEL --


type alias Model =
    { gameState : Game.Model }


getDimensions : ( Float, Float )
getDimensions =
    ( 600, 400 )


initialModel : Model
initialModel =
    { gameState = Game.initialModel getDimensions }



-- INIT --


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



-- UPDATE --


type Msg
    = GameMsg Game.Msg


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        GameMsg subMsg ->
            let
                gameModel =
                    Game.update subMsg model.gameState
            in
            ( { model | gameState = gameModel }, Cmd.none )



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map GameMsg <|
            Game.subscriptions model.gameState
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

module Input exposing (Model, Msg, initialModel, subscriptions, update)

import Browser.Events
import Json.Decode as Decode



-- MODEL --


type alias Model =
    { space : Bool
    , paddle1 : Int
    , paddle2 : Int
    }


initialModel : Model
initialModel =
    { space = False, paddle1 = 0, paddle2 = 0 }



-- UPDATE --


type Key
    = ArrowUp
    | ArrowDown


type Msg
    = KeyDown Key
    | KeyUp Key


update : Msg -> Model -> Model
update msg model =
    case msg of
        KeyDown key ->
            handleKeyDown key model

        KeyUp key ->
            handleKeyUp key model


handleKeyDown : Key -> Model -> Model
handleKeyDown key model =
    case key of
        ArrowUp ->
            { model | paddle1 = -1 }

        ArrowDown ->
            { model | paddle1 = 1 }


handleKeyUp : Key -> Model -> Model
handleKeyUp key model =
    case key of
        ArrowUp ->
            { model | paddle1 = 0 }

        ArrowDown ->
            { model | paddle1 = 0 }



-- SUBSCRIPTIONS --


keyDecoder : Decode.Decoder Key
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "ArrowUp" ->
                        Decode.succeed ArrowUp

                    "ArrowDown" ->
                        Decode.succeed ArrowDown

                    _ ->
                        let
                            _ =
                                Debug.log "unknown key" string
                        in
                        Decode.fail "Invalid Key"
            )


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Browser.Events.onKeyDown (Decode.map KeyDown keyDecoder)
        , Browser.Events.onKeyUp (Decode.map KeyUp keyDecoder)
        ]

module Main exposing (..)

import Browser
import Html
import Http
import MastermindData exposing (..)
import MastermindView exposing (..)



-- Model


init : () -> ( Model, Cmd Msg )
init _ =
    ( emptyModel, sendRequest gameAPIUrl GotData Nothing )


type alias Model =
    { dataState : DataState
    , gameData : GameData
    , currentGuess : ColorSet
    }


type DataState
    = Loading
    | Failure
    | Success


emptyModel : Model
emptyModel =
    { dataState = Loading, gameData = emptyGameData, currentGuess = [] }



-- Update


type Msg
    = GotData (Result Http.Error RawGameData)
    | Guess Color
    | Clear
    | NewGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData result ->
            case result of
                Ok rawData ->
                    ( { model
                        | dataState = Success
                        , gameData = convertRawDataToGameData rawData
                        , currentGuess = []
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | dataState = Failure }, Cmd.none )

        Guess c ->
            case c of
                NoColor ->
                    -- if has 4 colors, should guess
                    if model.gameData.state == InGame && List.length model.currentGuess >= 4 then
                        ( { model | dataState = Loading }
                        , sendRequest
                            gameAPIUrl
                            GotData
                            (Just (getBodyForGuessAPIRequest model.gameData model.currentGuess))
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    if List.length model.currentGuess >= 4 then
                        ( model, Cmd.none )

                    else
                        ( { model | currentGuess = c :: model.currentGuess }, Cmd.none )

        Clear ->
            ( { model | currentGuess = [] }, Cmd.none )

        NewGame ->
            -- should basically start from beginning
            init ()



-- View


view : Model -> Html.Html Msg
view model =
    case model.dataState of
        -- Loading ->
        --     genAppHtml NewGame Clear Guess model.gameData model.currentGuess
        -- Success ->
        --     genAppHtml NewGame Clear Guess model.gameData model.currentGuess
        Failure ->
            Html.text "Something went wrong :( try reloading page."

        _ ->
            genAppHtml NewGame Clear Guess model.gameData model.currentGuess



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

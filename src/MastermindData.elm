module MastermindData exposing (..)

import Array
import Http
import Json.Decode as Decode
import Json.Encode as Encode



-- Constants


gameLen : Int
gameLen =
    7


gameAPIUrl : String
gameAPIUrl =
    "http://localhost:8080/mastermind"



-- Types


type alias RawGameData =
    { won : Bool
    , answer : List Int
    , guesses : List (List Int)
    , results : List (List Int)
    }


type alias GameData =
    { state : GameState
    , answer : ColorSet
    , guesses : Array.Array ColorSet
    , results : Array.Array GuessResultsSet
    }


emptyGameData : GameData
emptyGameData =
    { state = InGame
    , answer = []
    , guesses = Array.fromList []
    , results = Array.fromList []
    }


type GameState
    = InGame
    | Won
    | Lost


getGameState : RawGameData -> GameState
getGameState raw =
    if raw.won then
        Won

    else if not raw.won && List.length raw.guesses < gameLen then
        InGame

    else
        Lost


getBoolFromGameState : GameState -> Bool
getBoolFromGameState gs =
    case gs of
        Won ->
            True

        _ ->
            False


type alias ColorSet =
    List Color


type Color
    = NoColor
    | Red
    | Orange
    | Yellow
    | Green
    | Blue
    | Purple


getColorFromInt : Int -> Color
getColorFromInt i =
    case i of
        1 ->
            Red

        2 ->
            Orange

        3 ->
            Yellow

        4 ->
            Green

        5 ->
            Blue

        6 ->
            Purple

        _ ->
            NoColor


getIntFromColor : Color -> Int
getIntFromColor c =
    case c of
        Red ->
            1

        Orange ->
            2

        Yellow ->
            3

        Green ->
            4

        Blue ->
            5

        Purple ->
            6

        NoColor ->
            -1


type alias GuessResultsSet =
    List GuessResult


type GuessResult
    = NoGameResult
    | White
    | Black


getGuessResultFromInt : Int -> GuessResult
getGuessResultFromInt i =
    case i of
        1 ->
            White

        2 ->
            Black

        _ ->
            NoGameResult


getIntFromGameResult : GuessResult -> Int
getIntFromGameResult res =
    case res of
        White ->
            1

        Black ->
            2

        NoGameResult ->
            -1



-- Data requests


{-| Converts RawGameData ("raw" json data) -> GameData (used in this program)
-}
convertRawDataToGameData : RawGameData -> GameData
convertRawDataToGameData raw =
    let
        getColorSetFromInts : List Int -> ColorSet
        getColorSetFromInts l =
            List.map getColorFromInt l

        getResultSetFromInts : List Int -> GuessResultsSet
        getResultSetFromInts l =
            List.map getGuessResultFromInt l
    in
    { state = getGameState raw
    , answer = getColorSetFromInts raw.answer
    , guesses = List.map getColorSetFromInts raw.guesses |> Array.fromList
    , results = List.map getResultSetFromInts raw.results |> Array.fromList
    }


getBodyForGuessAPIRequest : GameData -> ColorSet -> Http.Body
getBodyForGuessAPIRequest game guess =
    let
        getEncodeValueFromColor : Color -> Encode.Value
        getEncodeValueFromColor c =
            getIntFromColor c |> Encode.int

        getGuessesValueFromColorSet : ColorSet -> Encode.Value
        getGuessesValueFromColorSet cs =
            Encode.list getEncodeValueFromColor cs

        getEncodeValueFromGameResult : GuessResult -> Encode.Value
        getEncodeValueFromGameResult res =
            getIntFromGameResult res |> Encode.int

        getResultValueFromGameResultSet : GuessResultsSet -> Encode.Value
        getResultValueFromGameResultSet grs =
            Encode.list getEncodeValueFromGameResult grs

        getJsonValueFromGameData : Encode.Value
        getJsonValueFromGameData =
            Encode.object
                [ ( "won", Encode.bool (getBoolFromGameState game.state) )
                , ( "answer", Encode.list getEncodeValueFromColor game.answer )
                , ( "guesses", Encode.array getGuessesValueFromColorSet game.guesses )
                , ( "results", Encode.array getResultValueFromGameResultSet game.results )
                ]

        getGuessJsonData : Encode.Value
        getGuessJsonData =
            Encode.object
                [ ( "game", getJsonValueFromGameData )
                , ( "guess", Encode.list getEncodeValueFromColor (List.reverse guess) )
                ]
    in
    Http.jsonBody getGuessJsonData


{-| If has body => POST else GET
-}
sendRequest :
    String
    -> (Result Http.Error RawGameData -> msg)
    -> Maybe Http.Body
    -> Cmd msg
sendRequest url successMsg mBody =
    let
        rawDataDecoder : Decode.Decoder RawGameData
        rawDataDecoder =
            Decode.map4 RawGameData
                (Decode.field "won" Decode.bool)
                (Decode.field "answer" (Decode.list Decode.int))
                (Decode.field "guesses" (Decode.list (Decode.list Decode.int)))
                (Decode.field "results" (Decode.list (Decode.list Decode.int)))

        expect : Http.Expect msg
        expect =
            Http.expectJson successMsg rawDataDecoder
    in
    case mBody of
        Just body ->
            Http.post
                { url = url
                , expect = expect
                , body = body
                }

        Nothing ->
            Http.get
                { url = url
                , expect = expect
                }

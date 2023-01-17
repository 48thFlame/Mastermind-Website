module MastermindView exposing (..)

import Array
import Html
import Html.Attributes exposing (class, href)
import Html.Events as Events
import MastermindData exposing (..)


genAppHtml :
    msg
    -> msg
    -> (Color -> msg)
    -> GameData
    -> ColorSet
    -> Html.Html msg
genAppHtml newGameMsg clearMsg guessMsg game currentGuess =
    Html.div
        [ class "app" ]
        [ title
        , generateGameHtml newGameMsg clearMsg guessMsg game currentGuess
        , credits
        ]


title : Html.Html msg
title =
    Html.h1 [] [ Html.text "Mastermind" ]


credits : Html.Html msg
credits =
    let
        userNameLink =
            "https://github.com/48thFlame/"

        repoLink =
            "https://github.com/48thFlame/Mastermind-Website"
    in
    Html.div []
        [ Html.hr [] []
        , Html.h2 [] [ Html.text "Credits" ]
        , Html.p
            []
            [ Html.text "Mastermind game by "
            , Html.a
                [ href userNameLink ]
                [ Html.text "48thFlame" ]
            , Html.text " "
            , Html.a
                [ href repoLink ]
                [ Html.text "Repo" ]
            ]
        ]


generateGameHtml :
    msg
    -> msg
    -> (Color -> msg)
    -> GameData
    -> ColorSet
    -> Html.Html msg
generateGameHtml newGameMsg clearMsg guessMsg game currentGuess =
    Html.div [ class "game_section" ]
        [ generateGameBoard game

        -- , Html.br [] []
        , generateCurrentGuess game currentGuess
        , generateGameColorButtons guessMsg
        , generateClearButton clearMsg
        , generateGuessButton guessMsg
        , Html.br [] []

        -- , Html.br [] []
        , generateNewGameButton newGameMsg
        , Html.br [] []
        , Html.br [] []
        ]


generateGameColorButtons : (Color -> msg) -> Html.Html msg
generateGameColorButtons guessMsg =
    let
        guessingColors : List Color
        guessingColors =
            [ Red, Orange, Yellow, Green, Blue, Purple ]

        colorButton c =
            Html.button
                [ class "guess_color_button", Events.onClick (guessMsg c) ]
                [ Html.text (colorToString c) ]
    in
    Html.div [] (List.map colorButton guessingColors)


generateGuessButton : (Color -> msg) -> Html.Html msg
generateGuessButton guessMsg =
    Html.button
        [ class "game_control_button", class "guess_button", Events.onClick (guessMsg NoColor) ]
        [ Html.text "Guess ➔" ]


generateClearButton : msg -> Html.Html msg
generateClearButton clearMsg =
    Html.button
        [ class "game_control_button", class "clear_button", Events.onClick clearMsg ]
        [ Html.text "Clear 🗙" ]


generateNewGameButton : msg -> Html.Html msg
generateNewGameButton newGameMsg =
    Html.button
        [ class "game_control_button", class "new_game_button", Events.onClick newGameMsg ]
        [ Html.text "New game 🗘" ]


generateGameBoard : GameData -> Html.Html msg
generateGameBoard game =
    -- (List.range 0 (gameLen - 1) |> List.map gameRow)
    Html.div []
        (List.map (generateGameRow game) (List.range 0 (gameLen - 1)))


generateGameRow : GameData -> Int -> Html.Html msg
generateGameRow game roundI =
    let
        guessResultGameBoardSep : String
        guessResultGameBoardSep =
            " ------ "
    in
    Html.div []
        [ Html.div []
            [ Html.span []
                [ Html.text ("Round " ++ String.fromInt (roundI + 1) ++ ":")
                ]
            , Html.br [] []
            , Html.span []
                [ Html.text
                    ((Array.get roundI game.guesses
                        |> getColorSetFromMaybe
                        |> List.map colorToString
                        |> String.join ""
                     )
                        ++ guessResultGameBoardSep
                        ++ (Array.get roundI game.results
                                |> getGameResultSetFromMaybe
                                |> List.map gameResultToString
                                |> String.join ""
                           )
                    )
                ]
            ]
        ]


generateCurrentGuess : GameData -> ColorSet -> Html.Html msg
generateCurrentGuess game currentGuess =
    let
        playing : Bool
        playing =
            game.state == InGame

        -- {-| Makes currentGuess 4 long, fills non guessed yet color with `NoColor`-}
        make4Long : ColorSet
        make4Long =
            List.append
                (List.reverse currentGuess)
                (List.repeat
                    (4 - List.length currentGuess)
                    NoColor
                )
    in
    Html.div []
        [ if playing then
            Html.text
                ("Current guess: "
                    ++ (make4Long
                            |> List.map colorToString
                            |> String.join ""
                       )
                )

          else
            case game.state of
                Won ->
                    Html.h3 []
                        [ Html.text "You Won! :)" ]

                Lost ->
                    Html.h3 []
                        [ Html.text "You lost :(" ]

                _ ->
                    Html.text ""
        ]


getColorSetFromMaybe : Maybe ColorSet -> ColorSet
getColorSetFromMaybe mcs =
    case mcs of
        Just cs ->
            cs

        Nothing ->
            List.repeat 4 NoColor


getGameResultSetFromMaybe : Maybe GuessResultsSet -> GuessResultsSet
getGameResultSetFromMaybe mgrs =
    case mgrs of
        Just grs ->
            grs

        Nothing ->
            List.repeat 4 NoGameResult


colorToString : Color -> String
colorToString c =
    case c of
        Red ->
            "🟥"

        Orange ->
            "🟧"

        Yellow ->
            "🟨"

        Green ->
            "🟩"

        Blue ->
            "🟦"

        Purple ->
            "🟪"

        NoColor ->
            -- "🔳"
            "➖"


gameResultToString : GuessResult -> String
gameResultToString gr =
    case gr of
        White ->
            -- "❎"
            "⚪"

        Black ->
            -- "✅"
            "⚫"

        NoGameResult ->
            "➖"

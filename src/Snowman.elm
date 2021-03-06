module Snowman exposing (init)

import Browser exposing (Document)
import Browser.Events exposing (onKeyPress)
import Char exposing (toUpper)
import Html exposing (Html, a, button, code, div, h1, input, p, pre, span, text)
import Html.Attributes exposing (alt, class, disabled, href, src)
import Html.Attributes.Extra
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Random exposing (Generator)
import Random.List exposing (choose)
import String exposing (concat, contains, fromChar, fromList, join, lines, toUpper, trim, uncons)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



---- MODEL ----


type alias Model =
    { state : GameState
    , secretWord : String
    , letter : List Char
    , errors : Int
    , remainingWords : List String
    }


type GameState
    = ReadyToPlay
    | Playing
    | Won
    | Lost


initModel : Model
initModel =
    { state = ReadyToPlay
    , secretWord = "INITIAL"
    , letter = []
    , errors = 0
    , remainingWords = []
    }


initialSecretWordList : List String
initialSecretWordList =
    List.map String.toUpper [ "disastrous", "forgetful", "clumsy", "obnoxious", "defiant", "malicious", "trousers", "wobble", "adaptable", "hideous" ]


init : flags -> ( Model, Cmd Msg )
init _ =
    ( initModel, chooseWord initialSecretWordList )


maxAttempts : Int
maxAttempts =
    6


alphabet : List Char
alphabet =
    [ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z' ]



---- UPDATE ----


type Msg
    = UpdateSnowman Char
    | Otherkey
    | Initialize ( Maybe String, List String )
    | Restart


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        appendIfNotPresent : Char -> List Char
        appendIfNotPresent c =
            if List.member (Char.toUpper c) model.letter then
                model.letter

            else
                model.letter ++ [ Char.toUpper c ]

        incErrorIfNotInWord : Char -> Int
        incErrorIfNotInWord c =
            if String.contains (fromChar (Char.toUpper c)) model.secretWord then
                model.errors

            else
                model.errors + 1

        modelWon : Model -> Bool
        modelWon aModel =
            let
                maskedSecretWord : String
                maskedSecretWord =
                    maskString aModel.secretWord aModel.letter

                maskedChars : List Char
                maskedChars =
                    String.toList maskedSecretWord

                containsMaskSymbol : Bool
                containsMaskSymbol =
                    contains maskSymbol maskedChars
            in
            containsMaskSymbol == False

        modelLost : Model -> Bool
        modelLost aModel =
            aModel.errors >= maxAttempts

        newGameState : Model -> GameState
        newGameState aModel =
            if modelWon aModel then
                Won

            else if modelLost aModel then
                Lost

            else
                Playing
    in
    case message of
        UpdateSnowman c ->
            if isWon model || isLost model then
                ( model, Cmd.none )

            else if contains c alphabet then
                let
                    newModel : Model
                    newModel =
                        { model | letter = appendIfNotPresent c, errors = incErrorIfNotInWord c }
                in
                ( { newModel | state = newGameState newModel }, Cmd.none )

            else
                ( model, Cmd.none )

        Otherkey ->
            ( model, Cmd.none )

        Initialize ( maybeNewWord, remainingWords ) ->
            case maybeNewWord of
                Just newWord ->
                    ( { initModel | secretWord = newWord, remainingWords = remainingWords }, Cmd.none )

                Nothing ->
                    ( { initModel | secretWord = "FALLBACK" }, Cmd.none )

        Restart ->
            ( model, chooseWord model.remainingWords )



--- SUBSCRIPTIONS ---


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown keyDecoder



---- VIEW ----


view : Model -> Document Msg
view model =
    { title = "TTT Snowman"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    div
        [ class "terminal" ]
        [ div [ class "container" ]
            [ viewContent model ]
        ]


viewContent : Model -> Html Msg
viewContent model =
    div []
        [ viewHeader
        , viewChallenge model
        , viewHistory model
        , viewAlphabetButtons model
        , viewSnowman model
        , viewResultMessage model
        , viewRestartButton model
        , viewFooter
        ]



---- HELPER ----


contains : Char -> List Char -> Bool
contains c cs =
    String.contains (String.fromChar c) (fromList cs)


typedChar : Maybe ( Char, String ) -> Char
typedChar maybe =
    case maybe of
        Just ( c, _ ) ->
            c

        Nothing ->
            emptyChar


emptyChar : Char
emptyChar =
    Char.fromCode 0


isWon : Model -> Bool
isWon model =
    case model.state of
        Won ->
            True

        _ ->
            False


isLost : Model -> Bool
isLost model =
    case model.state of
        Lost ->
            True

        _ ->
            False


maskSymbol : Char
maskSymbol =
    '_'


maskIfNotPresent : Char -> List Char -> Char
maskIfNotPresent c cs =
    if List.member (Char.toUpper c) cs then
        Char.toUpper c

    else
        '_'


maskString : String -> List Char -> String
maskString word letters =
    let
        wordChars : List Char
        wordChars =
            String.toList word

        mask : Char -> Char
        mask c =
            maskIfNotPresent c letters

        maskedWordChars : List Char
        maskedWordChars =
            List.map mask wordChars
    in
    String.fromList maskedWordChars


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            UpdateSnowman (Char.toUpper char)

        _ ->
            Otherkey


chooseWord : List String -> Cmd Msg
chooseWord words =
    choose initialSecretWordList
        |> Random.generate Initialize



---- VIEW HELPER ----


viewHeader : Html msg
viewHeader =
    h1 [] [ text "TTT Snowman" ]


viewChallenge : Model -> Html Msg
viewChallenge model =
    p []
        [ span [] (viewChallengeInfo :: viewSecretWord model)
        ]


viewChallengeInfo : Html Msg
viewChallengeInfo =
    span [ class "info" ] [ text "Secret word:" ]


viewSecretWord : Model -> List (Html Msg)
viewSecretWord model =
    let
        maskedSecretWord : String
        maskedSecretWord =
            maskString model.secretWord model.letter

        toSpan : Char -> Html Msg
        toSpan c =
            span [ class "letter" ] [ text (fromChar c) ]

        toSpanList : String -> List (Html Msg)
        toSpanList word =
            case uncons word of
                Just ( c, cs ) ->
                    toSpan c :: toSpanList cs

                Nothing ->
                    []
    in
    toSpanList maskedSecretWord


viewHistory : Model -> Html Msg
viewHistory model =
    p []
        [ span [ class "terminal-prompt" ] (viewHistoryInfo :: viewLetters model)
        ]


viewHistoryInfo : Html Msg
viewHistoryInfo =
    span [ class "info" ] [ text "Typed letters:" ]


viewLetters : Model -> List (Html Msg)
viewLetters model =
    let
        toSpan : Char -> Html Msg
        toSpan c =
            span [ class "letter" ] [ text (fromChar c) ]

        toSpanList : List Char -> List (Html Msg)
        toSpanList cs =
            List.map toSpan cs
    in
    toSpanList model.letter


viewAlphabetButtons : Model -> Html Msg
viewAlphabetButtons model =
    let
        toButton : Char -> Html Msg
        toButton c =
            button
                [ onClick (UpdateSnowman c)
                , class "btn"
                , class "btn-default"
                , disabled (contains c model.letter)
                ]
                [ text (fromChar c) ]
    in
    div [ class "char-button-list" ] (List.map toButton alphabet)


viewResultMessage : Model -> Html Msg
viewResultMessage model =
    let
        attemptsLeft : String
        attemptsLeft =
            String.fromInt (maxAttempts - model.errors)
    in
    case model.state of
        ReadyToPlay ->
            p [] [ text "Type or press a letter to start the game (", text attemptsLeft, text " attempts left)" ]

        Won ->
            p [] [ text "You won the game ????" ]

        Lost ->
            p [] [ text "You lost the game ????. The secret word was: ", text model.secretWord ]

        Playing ->
            p [] [ text attemptsLeft, text " attempts left" ]


viewRestartButton : Model -> Html Msg
viewRestartButton model =
    p [] [ button [ onClick Restart ] [ text "Restart" ] ]


viewFooter : Html Msg
viewFooter =
    p []
        [ text "Made with ?????? and "
        , a [ href "https://elm-lang.org" ] [ text "Elm" ]
        , text " for viesure. View source on "
        , a [ href "https://github.com/mpgirro/ttt-snowman" ] [ text "GitHub" ]
        , text "."
        ]


viewSnowman : Model -> Html msg
viewSnowman model =
    let
        snowmanList : List String
        snowmanList =
            case model.errors of
                0 ->
                    [ snowmanEmptyText4
                    , snowmanEmptyText5
                    , snowmanEmptyText5
                    , snowmanEmptyText5
                    , snowmanEmptyText5
                    , snowmanEmptyText4
                    ]

                1 ->
                    [ snowmanEmptyText4
                    , snowmanEmptyText5
                    , snowmanEmptyText5
                    , snowmanEmptyText5
                    , snowmanEmptyText5
                    , snowmanText6
                    ]

                2 ->
                    [ snowmanText1
                    , snowmanEmptyText5
                    , snowmanEmptyText5
                    , snowmanEmptyText5
                    , snowmanEmptyText5
                    , snowmanText6
                    ]

                3 ->
                    [ snowmanText1
                    , snowmanEmptyText5
                    , snowmanEmptyText5
                    , snowmanEmptyText5
                    , snowmanText5
                    , snowmanText6
                    ]

                4 ->
                    [ snowmanText1
                    , snowmanText2
                    , snowmanEmptyText5
                    , snowmanEmptyText5
                    , snowmanText5
                    , snowmanText6
                    ]

                5 ->
                    [ snowmanText1
                    , snowmanText2
                    , snowmanEmptyText5
                    , snowmanText4
                    , snowmanText5
                    , snowmanText6
                    ]

                _ ->
                    [ snowmanText1
                    , snowmanText2
                    , snowmanText3
                    , snowmanText4
                    , snowmanText5
                    , snowmanText6
                    ]

        snowman : String
        snowman =
            join "\n" snowmanList
    in
    pre [ class "snowman" ] [ text snowman ]


snowmanEmptyText4 : String
snowmanEmptyText4 =
    "         "


snowmanEmptyText5 : String
snowmanEmptyText5 =
    "         "


snowmanText1 : String
snowmanText1 =
    "     __"


snowmanText2 : String
snowmanText2 =
    "   _|==|_ "


snowmanText3 : String
snowmanText3 =
    "    ('')___/"


snowmanText4 : String
snowmanText4 =
    ">--(`^^')"


snowmanText5 : String
snowmanText5 =
    "  (`^'^'`)"


snowmanText6 : String
snowmanText6 =
    "  `======'"

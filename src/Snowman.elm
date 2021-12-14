module Snowman exposing (init)

import Browser exposing (Document)
import Browser.Events exposing (onKeyPress)
import Char exposing (toUpper)
import Html exposing (Html, button, code, div, h1, input, p, pre, span, text)
import Html.Attributes exposing (alt, class, href, src)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
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
    { snowman : Snowman
    , secretWord : String
    , letter : List Char
    , errors : Int
    }


type Snowman
    = EmptySnowman


initModel : Model
initModel =
    { snowman = EmptySnowman
    , secretWord = randomWord
    , letter = []
    , errors = 0
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )



---- UPDATE ----


type Msg
    = UpdateSnowman Char
    | Otherkey
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
    in
    case message of
        UpdateSnowman c ->
            if contains c alphabet then
                ( { model | letter = appendIfNotPresent c, errors = incErrorIfNotInWord c }, Cmd.none )

            else
                ( model, Cmd.none )

        Otherkey ->
            ( model, Cmd.none )

        Restart ->
            ( initModel, Cmd.none )



--- SUBSCRIPTIONS ---


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown keyDecoder


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



---- VIEW ----


view : Model -> Document Msg
view model =
    { title = "TTT Snowman"
    , body = [ viewBody model ]
    }


viewContent : Model -> Html Msg
viewContent model =
    div []
        [ viewHeader
        , viewHistory model
        , viewAlphabetButtons
        , viewChallenge model
        , viewSnowman model
        , viewRestartButton

        --        , viewFooter
        ]


viewBody : Model -> Html Msg
viewBody model =
    div
        [ class "terminal" ]
        [ div [ class "container" ]
            [ viewContent model ]
        ]



---- HELPER ----


randomWord : String
randomWord =
    "ERNSTL"


contains : Char -> List Char -> Bool
contains c cs =
    String.contains (String.fromChar c) (fromList cs)


alphabet : List Char
alphabet =
    [ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z' ]


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


maxAttempts : Int
maxAttempts =
    6



---- VIEW HELPER ----


viewHeader : Html msg
viewHeader =
    h1 [] [ text "TTT Snowman" ]


viewChallenge : Model -> Html Msg
viewChallenge model =
    div []
        [ span [] (viewChallengeInfo :: viewSecretWord model)
        ]


viewChallengeInfo : Html Msg
viewChallengeInfo =
    span [ class "info" ] [ text "Guess the word:" ]


viewAlphabetButtons : Html Msg
viewAlphabetButtons =
    let
        toButton : Char -> Html Msg
        toButton c =
            button [ onClick (UpdateSnowman c), class "btn", class "btn-default" ] [ text (fromChar c) ]
    in
    div [ class "char-button-list" ] (List.map toButton alphabet)


viewHistory : Model -> Html Msg
viewHistory model =
    div []
        [ span [ class "terminal-prompt" ] (viewHistoryInfo :: viewLetters model)
        ]


viewHistoryInfo : Html Msg
viewHistoryInfo =
    span [ class "info" ] [ text "Last typed letters:" ]


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


viewRestartButton : Html Msg
viewRestartButton =
    button [ onClick Restart ] [ text "Restart" ]


viewFooter : Html Msg
viewFooter =
    div [] [ text "Made by Max with ❤️ and Elm for viesure. View source on GitHub." ]


viewSecretWord : Model -> List (Html Msg)
viewSecretWord model =
    let
        mask : Char -> String
        mask c =
            if List.member (Char.toUpper c) model.letter then
                String.toUpper (fromChar c)

            else
                "_"

        toSpan : Char -> Html Msg
        toSpan c =
            span [ class "letter" ] [ text (mask c) ]

        toSpanList : String -> List (Html Msg)
        toSpanList foo =
            let
                bar =
                    uncons foo
            in
            case bar of
                Just ( c, cs ) ->
                    toSpan c :: toSpanList cs

                Nothing ->
                    []
    in
    toSpanList model.secretWord


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
                    [ snowmanEmptyText4
                    , snowmanEmptyText5
                    , snowmanEmptyText5
                    , snowmanEmptyText5
                    , snowmanText5
                    , snowmanText6
                    ]

                3 ->
                    [ snowmanEmptyText4
                    , snowmanEmptyText5
                    , snowmanEmptyText5
                    , snowmanText4
                    , snowmanText5
                    , snowmanText6
                    ]

                4 ->
                    [ snowmanEmptyText4
                    , snowmanEmptyText5
                    , snowmanText3
                    , snowmanText4
                    , snowmanText5
                    , snowmanText6
                    ]

                5 ->
                    [ snowmanEmptyText4
                    , snowmanText2
                    , snowmanText3
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

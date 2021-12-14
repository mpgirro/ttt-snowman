module Snowman exposing (..)

import Browser exposing (Document)
import Browser.Events exposing (onKeyPress)
import Char exposing (toUpper)
import Html exposing (Html, button, code, div, h1, input, p, pre, span, text)
import Html.Attributes exposing (..)
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
                ( { model | letter = appendIfNotPresent c, errors = incErrorIfNotInWord c }, Cmd.none)
            else
                ( model, Cmd.none )

        Otherkey ->
            ( model, Cmd.none )

        Restart ->
            ( initModel, Cmd.none)


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
    , body = [ viewContent model ]
    }

viewContent : Model -> Html Msg
viewContent model =
    div []
        [ viewHeader
        , viewQuiz model
        , viewAlphabetButtons
        , viewHistory model
        , viewSnowman model
        , viewRestartButton
        , viewFooter
        ]

---- HELPER ----

randomWord : String
randomWord = "ERNSTL"


contains : Char -> List Char -> Bool
contains c cs =
    String.contains (String.fromChar c) (fromList cs)

alphabet : List Char
alphabet = 
    ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']
  

typedChar : Maybe ( Char, String ) -> Char 
typedChar maybe =
    case maybe of
        Just ( c, _ ) -> 
            c
        Nothing -> 
            emptyChar

emptyChar : Char
emptyChar = Char.fromCode 0


maxAttempts : Int
maxAttempts = 6

---- VIEW HELPER ----

viewHeader : Html msg
viewHeader = h1 [] [ text "TTT Snowman" ] 

viewQuiz : Model -> Html Msg
viewQuiz model = 
    div [] 
        [ p [] [ text "Guess the word", (viewSecretWord model) ] ]

viewAlphabetButtons: Html Msg
viewAlphabetButtons =
    let

        toButton : Char -> Html Msg
        toButton c =
            button [ onClick (UpdateSnowman c) ] [ text (fromChar c) ]
    in
    div [] (List.map toButton alphabet)

viewHistory : Model -> Html Msg
viewHistory model = 
    p [] 
    [ text "Last typed letters:", viewLetters model ]


viewLetters : Model -> Html Msg
viewLetters model =
    let
        toSpan : Char -> Html Msg
        toSpan c = span [] [ text (fromChar c) ]

        toSpanList : List Char -> List (Html Msg)
        toSpanList cs =
            List.map toSpan cs
    in
        div [] (toSpanList model.letter)

viewRestartButton : Html Msg
viewRestartButton =
    button [ onClick Restart ] [ text "Restart" ]


viewFooter : Html Msg
viewFooter = 
    div [] [ text "Made by Max with ❤️ and Elm for viesure. View source on GitHub." ]


viewSecretWord : Model -> Html Msg
viewSecretWord model =
    let
        mask : Char -> String
        mask c =
            if List.member (Char.toUpper c) model.letter then
                String.toUpper (fromChar c)
            else
                " _ "
        
        toSpan : Char -> Html Msg
        toSpan c =
            span [] [ text (mask c) ]

        toSpanList : String -> List (Html Msg)
        toSpanList foo =
            let
                bar = uncons foo
            in 
            case bar of
                Just (c, cs) ->
                    (toSpan c) :: (toSpanList cs)
                Nothing ->
                    []
    in
        div [] (toSpanList model.secretWord)


viewSnowman : Model -> Html msg
viewSnowman model =
    let
        filterEmptyLine : String -> String
        filterEmptyLine line =
            if line == "\n" then
                ""
            else 
                line

        snowmanList : List String
        snowmanList = 
            case model.errors of
                0 -> 
                    [ trim snowmanEmptyText4
                    , snowmanEmptyText5
                    , snowmanEmptyText5
                    , snowmanEmptyText5
                    , snowmanEmptyText5
                    , snowmanEmptyText4
                    ]
                1 -> 
                    [ snowmanText1
                    , snowmanEmptyText5
                    , snowmanEmptyText5
                    , snowmanEmptyText5
                    , snowmanEmptyText5
                    , snowmanEmptyText4
                    ]
                2 ->
                    [ snowmanText1
                    , snowmanText2
                    , snowmanEmptyText5
                    , snowmanEmptyText5
                    , snowmanEmptyText5
                    , snowmanEmptyText4
                    ]
                3 ->
                    [ snowmanText1
                    , snowmanText2
                    , snowmanText3
                    , snowmanEmptyText5
                    , snowmanEmptyText5
                    , snowmanEmptyText4
                    ]
                4 ->
                    [ snowmanText1
                    , snowmanText2
                    , snowmanText3
                    , snowmanText4
                    , snowmanEmptyText5
                    , snowmanEmptyText4
                    ]
                5 ->
                    [ snowmanText1
                    , snowmanText2
                    , snowmanText3
                    , snowmanText4
                    , snowmanText5
                    , snowmanEmptyText4
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
            join "\n" (List.map filterEmptyLine (lines (concat snowmanList)))

        in 
            pre [] [ text snowman ]

snowmanEmptyText4 : String
snowmanEmptyText4 =
    """




    """

snowmanEmptyText5 : String
snowmanEmptyText5 =
    """





    """

snowmanText1 : String
snowmanText1 = 
    """
                       .--------.
  *               .    |________|        .          *
                       |      __|/\\
            *        .-'======\\_\\o/. 
    """

snowmanText2 : String
snowmanText2 = 
    """
                    /___________<>__\\
              ||||||  /  (o) (o)  \\
              |||||| |   _  O  _   |          .
    .         |||||| |  (_)   (_)  |
              ||||||  \\   '---'   /    * 
    """

snowmanText3 : String
snowmanText3 = 
    """
              \\====/   [~~~~~~~~~]
               \\//  _/~||~`|~~~~~\\_
               _||-'`/  ||  |      \\`'-._       *
       *    .-` )|  ;   ||  |)      ;    '.
           /    `--.|   ||  |       |      `\\ 
    """

snowmanText4 : String
snowmanText4 = 
    """
          |         \\   |||||)      |-,      \\         .
           \\       .;       _       ; |_,    |
            `'''||` ,\\     (_)     /,    `.__/
                ||.`  '.         .'  `.             *
     *          ||       ` ' ' `       \\
    """

snowmanText5 : String
snowmanText5 = 
    """
                ||                      ;
  .          *  ||                      |    .
                ||                      |              *
                ||                      |
.__.-""-.__.-\"""||                      ;.-""\"-.__.-""-.__. 
    """

snowmanText6 : String
snowmanText6 = 
    """
                ||                     /
           jgs  ||'.                 .'
                ||  '-._  _ _  _ _.-'
               `""` 
    """

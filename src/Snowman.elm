module Snowman exposing (..)

import Browser exposing (Document)
import Browser.Events exposing (onKeyPress)
import Html exposing (Html, button, code, div, h1, input, p, pre, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String exposing (fromChar, fromList, uncons)
    

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
    , errors = 6
    }

init : flags -> ( Model, Cmd Msg )
init _ = 
    ( initModel, Cmd.none )
        
  

---- UPDATE ----


type Msg
    = UpdateSnowman Char
    | Restart


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        UpdateSnowman c ->
            ( { model | letter = model.letter ++ [c] }, Cmd.none)

        Restart ->
            ( initModel, Cmd.none)


--- SUBSCRIPTIONS ---


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



---- VIEW ----

updateInput : Model -> String -> Msg
updateInput model query =
    UpdateSnowman (typedChar (uncons query))
        

typedChar : Maybe ( Char, String ) -> Char 
typedChar maybe =
    case maybe of
        Just ( c, _ ) -> 
            c
        Nothing -> 
            emptyChar


emptyChar : Char
emptyChar = Char.fromCode 0

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
        , input [ placeholder "Guess the word", value "", onInput (updateInput model) ] []
        , viewHistory model
        , viewSnowman model
        , viewFooter
        ]


--viewContent : Model -> ( String, Html Msg )
--viewContent model = ( "TTT Snowman", foo)

viewHeader : Html msg
viewHeader = h1 [] [ text "TTT Snowman" ] 

viewQuiz : Model -> Html msg
viewQuiz model = 
    div [] 
        [ p [] [ span [] [ text "Quiz" ], text model.secretWord ] ]

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

viewFooter : Html Msg
viewFooter = 
    div [] [ text "Made with ❤️ and Elm for viesure by Max" ]

---- HELPER ----

randomWord : String
randomWord = "ERNSTL"


viewSnowman : Model -> Html msg
viewSnowman model =
    let
        snowman = 
            case model.errors of
                1 -> 
                    [ text snowmanText1
                    , text snowmanEmptyText5
                    , text snowmanEmptyText5
                    , text snowmanEmptyText5
                    , text snowmanEmptyText5
                    , text snowmanEmptyText4
                    ]
                2 ->
                    [ text snowmanText1
                    , text snowmanText2
                    , text snowmanEmptyText5
                    , text snowmanEmptyText5
                    , text snowmanEmptyText5
                    , text snowmanEmptyText4
                    ]
                3 ->
                    [ text snowmanText1
                    , text snowmanText2
                    , text snowmanText3
                    , text snowmanEmptyText5
                    , text snowmanEmptyText5
                    , text snowmanEmptyText4
                    ]
                4 ->
                    [ text snowmanText1
                    , text snowmanText2
                    , text snowmanText3
                    , text snowmanText4
                    , text snowmanEmptyText5
                    , text snowmanEmptyText4
                    ]
                5 ->
                    [ text snowmanText1
                    , text snowmanText2
                    , text snowmanText3
                    , text snowmanText4
                    , text snowmanText5
                    , text snowmanEmptyText4
                    ]
                6 ->
                    [ text snowmanText1
                    , text snowmanText2
                    , text snowmanText3
                    , text snowmanText4
                    , text snowmanText5
                    , text snowmanText6
                    ]
                _ -> [ text "ERROR" ]
        in 
            pre [] snowman

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
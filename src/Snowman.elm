module Snowman exposing (..)

import Browser exposing (Document)
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String exposing (fromList, uncons)
    

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
    , errorsLeft : Int
    }

type Snowman 
    = EmptySnowman

initModel : Model
initModel = 
    { snowman = EmptySnowman 
    , secretWord = randomWord
    , letter = []
    , errorsLeft = 6
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
    let
        bar = uncons query
        foo = typedChar bar
    in 
        UpdateSnowman foo

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
        [ div [] [ p [] [ text model.secretWord ] ]
        , input [ placeholder "Guess the word", value "", onInput (updateInput model) ] []
        , div [] [ text (fromList model.letter) ]
        ]


--viewContent : Model -> ( String, Html Msg )
--viewContent model = ( "TTT Snowman", foo)
    

---- HELPER ----

randomWord : String
randomWord = "ERNSTL"
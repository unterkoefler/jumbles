module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Json
import Random
import Random.List exposing (choose, shuffle)
import Words



-- MAIN


main =
    Browser.element 
        { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { word : String
    , jumbledWord : String
    , guess : String
    , message : String
    , words : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { word = "welcome"
      , jumbledWord = "emoclew"
      , guess = ""
      , message = ""
      , words = Words.words
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GuessChanged String
    | Check
    | KeyDown Int
    | Next
    | NextWord ( Maybe String, List String )
    | JumbledWord (List Char)
    | GiveUp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GuessChanged newGuess ->
            ( { model | guess = newGuess, message = "" }, Cmd.none )

        KeyDown key ->
            case key of
                13 ->
                    checkMatch model

                _ ->
                    ( model, Cmd.none )

        Check ->
            checkMatch model

        Next ->
            ( { model | message = "", word = "", guess = "" }
            , Random.generate NextWord (choose model.words)
            )

        NextWord ( maybeWord, wordsRemaining ) ->
            case maybeWord of
                Just word ->
                    ( { model | word = word, words = wordsRemaining }
                    , Random.generate JumbledWord (shuffle (String.toList word))
                    )

                Nothing ->
                    ( { model | message = "No words left :(" }, Cmd.none )

        JumbledWord jumbledChars ->
            ( { model | jumbledWord = String.fromList jumbledChars }, Cmd.none )

        GiveUp ->
            ( { model | message = "Stupid! The word was " ++ model.word }
            , Cmd.none
            )


checkMatch : Model -> ( Model, Cmd Msg )
checkMatch model =
    if match model.word model.guess then
        ( { model | message = "Correct!" }, Cmd.none )

    else
        ( { model | message = "Wrong! Try again!" }, Cmd.none )


match : String -> String -> Bool
match s1 s2 =
    String.toLower s1 == String.toLower s2


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text model.jumbledWord ]
        , viewInput "text" "Guess" model.guess GuessChanged
        , button [ onClick Check ] [ text "Check" ]
        , button [ onClick GiveUp ] [ text "Give up" ]
        , button [ onClick Next ] [ text "Next" ]
        , p [] [ text model.message ]
        ]


viewInput : String -> String -> String -> (String -> Msg) -> Html Msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg, onKeyDown KeyDown ] []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

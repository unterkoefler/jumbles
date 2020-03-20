module Main exposing (..)

import Browser exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Json.Decode as Json
import Random
import Random.List exposing (choose, shuffle)
import Words



-- MAIN


main : Program () Model Msg
main =
    Browser.document
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
    | Next
    | NextWord ( Maybe String, List String )
    | JumbledWord (List Char)
    | GiveUp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GuessChanged newGuess ->
            ( { model | guess = newGuess, message = "" }, Cmd.none )

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



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Jumbles"
    , body = [ getBody model ]
    }


getBody : Model -> Html Msg
getBody model =
    Element.layout
        [ Font.size 20
        , paddingEach { top = 30, right = 0, left = 15, bottom = 0 }
        ]
    <|
        Element.column
            [ width (px 800)
            , spacing 30
            , padding 10
            ]
            [ heading
            , row
                [ spacing 12
                ]
                [ text model.jumbledWord ]
            , Input.text
                [ spacing 12
                ]
                { label = Input.labelHidden "Guess"
                , onChange = GuessChanged
                , text = model.guess
                , placeholder = Just <| Input.placeholder [] (text "Guess")
                }
            , buttons
            , row
                [ spacing 12
                ]
                [ text model.message ]
            ]


heading : Element Msg
heading =
    el
        [ Region.heading 1
        , alignLeft
        , Font.size 36
        , spacing 20
        , Font.color (Element.rgb 0.2 0.34 0.98)
        ]
        (text "Jumbles")


buttons : Element Msg
buttons =
    column
        [ spacing 5
        , width (px 800)
        ]
        [ button Check "Check" (Element.rgb 0.4 0.78 0.4)
        , button GiveUp "Give up" (Element.rgb 0.55 0.3 0.8)
        , button Next "Next" (Element.rgb 0.4 0.78 0.8)
        ]


button : Msg -> String -> Element.Color -> Element Msg
button msg lbl color =
    Input.button
        [ Background.color color
        , paddingXY 32 16
        , Border.rounded 6
        , width fill
        ]
        { onPress = Just msg
        , label = Element.text lbl
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

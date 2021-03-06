module Main exposing (..)

import Browser exposing (Document)
import Browser.Events exposing (onResize)
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


main : Program Json.Value Model Msg
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
    , dimensions : Dimensions
    , score : Int
    , isRoundOver : Bool
    }


type alias Dimensions =
    { width : Int
    , height : Int
    }


init : Json.Value -> ( Model, Cmd Msg )
init flags =
    let
        dimensions =
            parseFlags flags
    in
    ( { word = "welcome"
      , jumbledWord = "emoclew"
      , guess = ""
      , message = ""
      , words = Words.words
      , dimensions = dimensions
      , score = 0
      , isRoundOver = False
      }
    , Cmd.none
    )


parseFlags : Json.Value -> Dimensions
parseFlags flags =
    Json.decodeValue dimensionsDecoder flags
        |> Result.withDefault defaultDimensions


dimensionsDecoder : Json.Decoder Dimensions
dimensionsDecoder =
    Json.map2 Dimensions
        (Json.field "width" Json.int)
        (Json.field "height" Json.int)


defaultDimensions =
    { width = 500
    , height = 500
    }



-- UPDATE


type Msg
    = GuessChanged String
    | Check
    | Next
    | NextWord ( Maybe String, List String )
    | JumbledWord (List Char)
    | GiveUp
    | WindowResized Dimensions


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GuessChanged newGuess ->
            ( { model | guess = newGuess, message = "" }, Cmd.none )

        Check ->
            checkMatch model

        Next ->
            ( { model | message = "", word = "", guess = "", isRoundOver = False }
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
            ( { model | message = "Stupid! The word was " ++ model.word, isRoundOver = True }
            , Cmd.none
            )

        WindowResized newDim ->
            ( { model | dimensions = newDim }, Cmd.none )


checkMatch : Model -> ( Model, Cmd Msg )
checkMatch model =
    if match model.word model.guess then
        ( { model
            | message = "Correct!"
            , score = incrementScore model
            , isRoundOver = True
          }
        , Cmd.none
        )

    else
        ( { model | message = "Wrong! Try again!" }, Cmd.none )


match : String -> String -> Bool
match s1 s2 =
    String.toLower s1 == String.toLower s2


incrementScore : Model -> Int
incrementScore model =
    if model.isRoundOver then
        model.score

    else
        model.score + 1



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Jumbles"
    , body = [ getBody model ]
    }


type ScreenSize
    = Mobile
    | Desktop


getScreenSize : Dimensions -> ScreenSize
getScreenSize dim =
    let
        { class, orientation } =
            classifyDevice dim
    in
    case ( class, orientation ) of
        ( Phone, Portrait ) ->
            Mobile

        ( Tablet, Portrait ) ->
            Mobile

        _ ->
            Desktop


baseFontSize =
    18


columnSpacing =
    30


basePadding =
    { top = 30, right = 5, left = 15, bottom = 0 }


baseSpacing =
    12


headingFontSize =
    2 * baseFontSize


buttonFontSize =
    24


headingSpacing =
    2 * baseSpacing


buttonSpacing =
    5


columnWidth : ScreenSize -> Int -> Length
columnWidth screenSize w =
    let
        max =
            w - (2 * (basePadding.left + basePadding.right))
    in
    case screenSize of
        Mobile ->
            px 400
                |> maximum max

        Desktop ->
            px 600
                |> maximum max


headingColor =
    Element.rgb 0.2 0.34 0.98


green =
    Element.rgb 0.4 0.78 0.4


purple =
    Element.rgb 0.61 0.33 0.88


teal =
    Element.rgb 0.4 0.78 0.8


getBody : Model -> Html Msg
getBody model =
    let
        screenSize =
            getScreenSize model.dimensions
    in
    Element.layout
        [ Font.size baseFontSize
        , paddingEach basePadding
        ]
    <|
        Element.column
            ([ width <| columnWidth screenSize model.dimensions.width
             , spacing columnSpacing
             ]
                ++ responsiveColumnAttributes screenSize
            )
            [ heading
            , row
                [ spacing baseSpacing
                , Font.letterSpacing 2
                ]
                [ text <| String.toUpper model.jumbledWord ]
            , Input.text
                [ spacing baseSpacing
                ]
                { label = Input.labelHidden "Guess"
                , onChange = GuessChanged
                , text = model.guess
                , placeholder = Just <| Input.placeholder [] (text "Guess")
                }
            , buttons screenSize model.dimensions
            , row
                [ spacing baseSpacing
                ]
                [ text model.message ]
            , row
                [ spacing baseSpacing
                ]
                [ text <| "Score: " ++ String.fromInt model.score ]
            ]


responsiveColumnAttributes : ScreenSize -> List (Attribute Msg)
responsiveColumnAttributes screenSize =
    case screenSize of
        Mobile ->
            []

        Desktop ->
            [ centerX ]


heading : Element Msg
heading =
    el
        [ Region.heading 1
        , alignLeft
        , Font.size headingFontSize
        , spacing headingSpacing
        , Font.color headingColor
        ]
        (text "Jumbles")


buttons : ScreenSize -> Dimensions -> Element Msg
buttons screenSize dim =
    let
        w =
            columnWidth screenSize dim.width

        group =
            case screenSize of
                Mobile ->
                    column

                Desktop ->
                    row
    in
    group
        [ spacing buttonSpacing
        , width w
        ]
        [ button Check "Check" green
        , button GiveUp "Give up" purple
        , button Next "Next" teal
        ]


button : Msg -> String -> Element.Color -> Element Msg
button msg lbl color =
    Input.button
        [ Background.color color
        , paddingXY 0 12
        , Font.center
        , Font.size buttonFontSize
        , Border.rounded 6
        , width fill
        ]
        { onPress = Just msg
        , label = Element.text lbl
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    onResize (\w h -> WindowResized { width = w, height = h })

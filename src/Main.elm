module Main exposing (..)

import Bootstrap.Alert exposing (simpleDanger, simpleSuccess)
import Browser
import Doubling.Doubling as Doubling
import Html
import Html.Styled exposing (Html, br, button, div, fromUnstyled, h2, input, span, text, toUnstyled)
import Html.Styled.Attributes exposing (class, css, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Html.Styled.Keyed as Keyed
import LargeSums.LargeSums as LargeSums
import Styling exposing (defaultMargin, mainWindow)
import SumTask exposing (Task)



-- MAIN


main =
    Browser.element { init = init, update = update, view = view >> toUnstyled, subscriptions = \_ -> Sub.none }



-- MODEL


type GameState
    = Init
    | Error String
    | LargeSumsState LargeSums.GameState
    | DoublingState Doubling.GameState


init : () -> ( GameState, Cmd Msg )
init _ =
    ( Init, Cmd.none )



-- UPDATE


type Msg
    = LargeSumsMsg LargeSums.Msg
    | DoublingMsg Doubling.Msg


update : Msg -> GameState -> ( GameState, Cmd Msg )
update commonMsg gameState =
    case ( commonMsg, gameState ) of
        ( LargeSumsMsg msg, LargeSumsState state ) ->
            let
                ( moduleState, moduleMsg ) =
                    LargeSums.update msg state
            in
            ( LargeSumsState moduleState, Cmd.map LargeSumsMsg moduleMsg )

        ( LargeSumsMsg LargeSums.Reset, Init ) ->
            let
                ( moduleState, moduleMsg ) =
                    LargeSums.init ()
            in
            ( LargeSumsState moduleState, Cmd.map LargeSumsMsg moduleMsg )

        ( DoublingMsg msg, DoublingState state ) ->
            let
                ( moduleState, moduleMsg ) =
                    Doubling.update msg state
            in
            ( DoublingState moduleState, Cmd.map DoublingMsg moduleMsg )

        ( DoublingMsg Doubling.Reset, Init ) ->
            let
                ( moduleState, moduleMsg ) =
                    Doubling.init ()
            in
            ( DoublingState moduleState, Cmd.map DoublingMsg moduleMsg )

        _ ->
            ( Error "Game state and message do not match. This should not happen.", Cmd.none )



-- VIEW


selectionButton : String -> Msg -> Html Msg
selectionButton name msg =
    span []
        [ button
            [ onClick msg
            , value name
            , css defaultMargin
            ]
            [ text name ]
        , br [] []
        ]


view : GameState -> Html Msg
view model =
    case model of
        LargeSumsState state ->
            Html.Styled.map LargeSumsMsg (LargeSums.view state)

        DoublingState state ->
            Html.Styled.map DoublingMsg (Doubling.view state)

        Init ->
            let
                contents =
                    div []
                        [ text "Bitte wähle eine Übung:"
                        , br [] []
                        , selectionButton "Verdoppeln" (DoublingMsg Doubling.Reset)
                        , selectionButton "Große Summen" (LargeSumsMsg LargeSums.Reset)
                        ]
            in
            mainWindow "Mathe" contents

        Error message ->
            h2 [] [ text (String.append "Error: " message) ]

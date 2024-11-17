module Main exposing (..)

import Browser
import Html.Styled exposing (Html, br, button, div, h2, span, text, toUnstyled)
import Html.Styled.Attributes exposing (class, css, value)
import Html.Styled.Events exposing (onClick)
import Modules.Division as Division exposing (division)
import Modules.Doubling as Doubling exposing (doubling)
import Modules.LargeSums as LargeSums exposing (largeSums)
import PracticeModule exposing (Msg(..), PracticeModule)
import Styling exposing (defaultMargin, mainWindow)



-- MAIN


main =
    Browser.element { init = init, update = update, view = view >> toUnstyled, subscriptions = \_ -> Sub.none }



-- MODEL


type GameState
    = Init
    | Error String
    | DoublingState Doubling.GameState
    | DivisionState Division.GameState
    | LargeSumsState LargeSums.GameState


init : () -> ( GameState, Cmd Msg )
init _ =
    ( Init, Cmd.none )



-- UPDATE


type Msg
    = DoublingMsg Doubling.Msg
    | DivisionMsg Division.Msg
    | LargeSumsMsg LargeSums.Msg


delegateUpdate : PracticeModule task -> (PracticeModule.GameState task -> GameState) -> (PracticeModule.Msg task -> Msg) -> PracticeModule.Msg task -> PracticeModule.GameState task -> ( GameState, Cmd Msg )
delegateUpdate mod wrapState wrapMessage msg state =
    let
        ( moduleState, moduleMsg ) =
            PracticeModule.update mod msg state
    in
    ( wrapState moduleState, Cmd.map wrapMessage moduleMsg )


initModule : PracticeModule task -> (PracticeModule.GameState task -> GameState) -> (PracticeModule.Msg task -> Msg) -> ( GameState, Cmd Msg )
initModule mod wrapState wrapMessage =
    let
        ( moduleState, moduleMsg ) =
            mod.init ()
    in
    ( wrapState moduleState, Cmd.map wrapMessage moduleMsg )


update : Msg -> GameState -> ( GameState, Cmd Msg )
update commonMsg gameState =
    case ( commonMsg, gameState ) of
        ( LargeSumsMsg msg, LargeSumsState state ) ->
            delegateUpdate largeSums LargeSumsState LargeSumsMsg msg state

        ( LargeSumsMsg Reset, Init ) ->
            initModule largeSums LargeSumsState LargeSumsMsg

        ( DoublingMsg msg, DoublingState state ) ->
            delegateUpdate doubling DoublingState DoublingMsg msg state

        ( DoublingMsg Reset, Init ) ->
            initModule doubling DoublingState DoublingMsg

        ( DivisionMsg msg, DivisionState state ) ->
            delegateUpdate division DivisionState DivisionMsg msg state

        ( DivisionMsg Reset, Init ) ->
            initModule division DivisionState DivisionMsg

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
            , class "btn btn-info"
            ]
            [ text name ]
        , br [] []
        ]


view : GameState -> Html Msg
view model =
    case model of
        LargeSumsState state ->
            Html.Styled.map LargeSumsMsg (PracticeModule.view largeSums state)

        DoublingState state ->
            Html.Styled.map DoublingMsg (PracticeModule.view doubling state)

        DivisionState state ->
            Html.Styled.map DivisionMsg (PracticeModule.view division state)

        Init ->
            let
                contents =
                    div []
                        [ text "Bitte wähle eine Übung:"
                        , br [] []
                        , selectionButton "Verdoppeln" (DoublingMsg Reset)
                        , selectionButton "Dividieren" (DivisionMsg Reset)
                        , selectionButton "Große Summen" (LargeSumsMsg Reset)
                        ]
            in
            mainWindow "Mathe" contents

        Error message ->
            h2 [] [ text (String.append "Error: " message) ]

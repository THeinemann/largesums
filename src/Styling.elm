module Styling exposing (defaultMargin, mainWindow)

import Css exposing (..)
import Html.Styled exposing (Html, div, h1, text)
import Html.Styled.Attributes exposing (css)


defaultMargin : List Style
defaultMargin =
    [ margin (px 3) ]


mainWindow : String -> Html msg -> Html msg
mainWindow title contents =
    div
        [ css
            [ marginLeft (pct 30)
            , marginRight (pct 30)
            , marginTop (px 20)
            , paddingBottom (px 12)
            , paddingTop (px 1)
            , textAlign center
            , backgroundColor (hex "#FFF")
            , boxShadow4 (px 0) (px 30) (px 60) (rgba 0 0 0 0.3)
            , borderRadius (px 12)
            ]
        ]
        [ h1 [ css [ paddingTop (px 4) ] ] [ text title ]
        , div [] [ contents ]
        ]

module App.Pages.NotFound exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)


view : Html msg
view =
    div [ class "page-content" ]
        [ div [ class "container text-center" ]
            [ div [ class "display-1 text-muted mb-5" ]
                [ i [ class "si si-exclamation" ] []
                , text "404"
                ]
            , h1 [ class "h2 mb-3" ] [ text "The page you are looking for doesn't exist." ]
            , p [ class "h4 text-muted font-weight-normal mb-7" ] [ text "Either you are not allowed to see this page or it doesn't exist." ]
            ]
        ]

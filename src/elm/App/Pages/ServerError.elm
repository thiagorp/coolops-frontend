module App.Pages.ServerError exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)


view : Html msg
view =
    div [ class "page-content" ]
        [ div [ class "container text-center" ]
            [ div [ class "display-1 text-muted mb-5" ]
                [ i [ class "si si-exclamation" ] []
                , text "500"
                ]
            , h1 [ class "h2 mb-3" ] [ text "Something bad happened." ]
            , p [ class "h4 text-muted font-weight-normal mb-7" ] [ text "We are currently having some problems but our team has already been alarmed and is working on a fix. Please try again later." ]
            ]
        ]

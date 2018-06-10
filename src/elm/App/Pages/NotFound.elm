module App.Pages.NotFound exposing (..)

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
            , h1 [ class "h2 mb-3" ] [ text "Oops.. You just found an error page.." ]
            , p [ class "h4 text-muted font-weight-normal mb-7" ] [ text "We are sorry but our service is currently not available&hellip;" ]
            ]
        ]

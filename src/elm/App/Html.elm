module App.Html exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


container : List (Html msg) -> Html msg
container children =
    div [ class "container" ] children


pageHeader : String -> Html msg
pageHeader title =
    div [ class "page-header" ]
        [ h1 [ class "page-title" ] [ text title ] ]


type Action msg
    = Create String msg


actionButton : Action msg -> Html msg
actionButton action =
    case action of
        Create buttonTitle clickHandler ->
            button [ class "btn btn-success", onClick clickHandler ]
                [ text buttonTitle ]


pageHeaderWithActions : String -> List (Action msg) -> Html msg
pageHeaderWithActions title actions =
    div [ class "page-header" ]
        [ h1 [ class "page-title" ] [ text title ]
        , div [ class "page-options d-flex" ]
            (List.map actionButton actions)
        ]


spinner : Html msg
spinner =
    div [ style [ ( "padding", "60px" ), ( "text-align", "center" ), ( "width", "100%" ) ] ]
        [ div [ class "spinner" ]
            [ div [ class "bounce1" ] []
            , div [ class "bounce2" ] []
            , div [ class "bounce3" ] []
            ]
        ]


fullscreenCard : List (Html msg) -> Html msg
fullscreenCard children =
    div [ class "row row-cards row-deck" ]
        [ div [ class "col-12" ]
            [ div [ class "card" ] children ]
        ]


table : List (Html msg) -> Html msg
table children =
    div [ class "table-responsive" ]
        [ Html.table [ class "table table-hover table-outline table-vcenter text-nowrap card-table" ] children ]


thead : List (Html msg) -> Html msg
thead =
    Html.thead []


tbody : List (Html msg) -> Html msg
tbody =
    Html.tbody []


tr : List (Html msg) -> Html msg
tr =
    Html.tr []


th : List (Html msg) -> Html msg
th =
    Html.th []


td : List (Html msg) -> Html msg
td =
    Html.td []


text : String -> Html msg
text =
    Html.text

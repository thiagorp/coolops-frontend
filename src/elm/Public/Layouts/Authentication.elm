module Public.Layouts.Authentication exposing (..)

import Form.Html exposing (Input, input, submitButton)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)


layout : List (Html msg) -> Html msg
layout children =
    div [ class "page-single" ]
        [ div [ class "container" ]
            [ div [ class "row" ]
                [ div [ class "col col-login mx-auto" ] children ]
            ]
        ]


form : Bool -> msg -> String -> Maybe String -> List (Input msg) -> Html msg
form submitting submitHandler title errorMessage inputs =
    let
        formTitle =
            [ div [ class "card-title" ] [ text title ] ]

        formInputs =
            List.map input inputs

        formErrorMessage =
            case errorMessage of
                Nothing ->
                    []

                Just error ->
                    [ p [ class "text-red" ] [ text error ] ]

        formButton =
            [ div [ class "form-footer" ] [ submitButton title submitting ] ]

        formBody =
            formTitle ++ formInputs ++ formErrorMessage ++ formButton
    in
    Html.form [ class "card", onSubmit submitHandler ]
        [ div [ class "card-body p-6" ] formBody ]

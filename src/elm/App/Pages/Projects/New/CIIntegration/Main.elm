module App.Pages.Projects.New.CIIntegration.Main exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Api
import App.Html as AppHtml
import App.Pages.Projects.New.CIIntegration.Data as Data
import App.Pages.ServerError as ServerError
import Html exposing (..)
import Html.Attributes exposing (..)
import Route
import Util exposing (PageHandler, andPerform, return)


type Msg
    = ApiResponse (Api.ApiResult (Maybe Data.Project))
    | LinkClicked Route.Route


type Remote
    = Loading
    | Failed
    | Success Data.Project


type alias Model =
    { project : Remote }


init : String -> String -> String -> PageHandler Model Msg
init baseUrl apiToken projectId =
    return { project = Loading }
        |> andPerform (Data.getData baseUrl apiToken projectId ApiResponse)


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        ApiResponse (Ok response) ->
            case response of
                Nothing ->
                    return model
                        |> andPerform (Route.redirectTo (Route.Protected (Route.NewProject Route.CreateProject)))

                Just project ->
                    return { model | project = Success project }

        ApiResponse (Err _) ->
            return { model | project = Failed }

        LinkClicked route ->
            return model
                |> andPerform (Route.redirectTo route)


view : Model -> Html Msg
view model =
    case model.project of
        Loading ->
            div [ class "card" ]
                [ div [ class "card-body" ] [ AppHtml.spinner ] ]

        Failed ->
            ServerError.view

        Success project ->
            div [ class "card" ]
                [ div [ class "card-body" ]
                    [ h2 [ class "h3" ] [ text "Pro hint" ]
                    , p [] [ text "If it's your first time integrating with CoolOps, test it on your own terminal beforehand so you get a feeling of what is possible to be done." ]
                    , h2 [ class "h3 mt-6 mb-4" ] [ text "1. Install the CLI" ]
                    , p []
                        [ text "CoolOps integrates with any CI workflow. Just follow the steps of our "
                        , a [ href "https://github.com/coolopsio/coolops" ] [ text "command line interface" ]
                        , text "."
                        ]
                    , h2 [ class "h3 mt-6 mb-4" ] [ text "2. Replace the access token" ]
                    , p []
                        [ text "Your project's access token is: "
                        , code [] [ text project.accessToken ]
                        ]
                    , p []
                        [ text "You can always find it on its settings page."
                        ]
                    , p []
                        [ text "Either pass it as the "
                        , code [] [ text "-t" ]
                        , text " flag of the "
                        , code [] [ text "build:new" ]
                        , text "'s command or assign it to the environment variable "
                        , code [] [ text "COOLOPS_PROJECT_API_TOKEN" ]
                        , text "."
                        ]
                    ]
                , div [ class "card-footer text-right" ]
                    [ AppHtml.a (Route.Protected Route.ProjectsList) LinkClicked [ href "#", class "btn btn-primary" ] [ text "Finish" ]
                    ]
                ]

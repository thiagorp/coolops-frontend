module App.Pages.Projects.New.Main exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

import Api
import App.Html as AppHtml
import App.Pages.Projects.New.CIIntegration.Main as CIIntegration
import App.Pages.Projects.New.CreateEnvironments.Main as CreateEnvironments
import App.Pages.Projects.New.CreateProject.Main as CreateProject
import App.Pages.Projects.New.SlackIntegration.Main as SlackIntegration
import Components.CheckmarkSteps as CheckmarkSteps
import Html exposing (..)
import Html.Attributes exposing (..)
import Route
import Util exposing (PageHandler, andPerform, noop, return)


type Msg
    = CreateProjectMsg CreateProject.Msg
    | SlackIntegrationMsg SlackIntegration.Msg
    | CreateEnvironmentsMsg CreateEnvironments.Msg
    | CIIntegrationMsg CIIntegration.Msg


type Step
    = CreateProject CreateProject.Model
    | SlackIntegration SlackIntegration.Model
    | CreateEnvironments CreateEnvironments.Model
    | CIIntegration CIIntegration.Model


type alias Model =
    { apiConfig : Api.ProtectedConfig
    , step : Step
    }


stepFromRoute : Api.ProtectedConfig -> Route.NavigationKey -> Route.NewProjectStep -> PageHandler Step Msg
stepFromRoute apiConfig navigationKey route =
    case route of
        Route.CreateProject ->
            CreateProject.init apiConfig navigationKey
                |> Util.map CreateProject CreateProjectMsg

        Route.IntegrateWithSlack projectId error ->
            SlackIntegration.init apiConfig navigationKey projectId error
                |> Util.map SlackIntegration SlackIntegrationMsg

        Route.CreateEnvironments projectId ->
            CreateEnvironments.init apiConfig navigationKey projectId
                |> Util.map CreateEnvironments CreateEnvironmentsMsg

        Route.IntegrateWithCI projectId ->
            CIIntegration.init apiConfig navigationKey projectId
                |> Util.map CIIntegration CIIntegrationMsg


init : Api.ProtectedConfig -> Route.NavigationKey -> Route.NewProjectStep -> PageHandler Model Msg
init apiConfig navigationKey route =
    let
        setModel step =
            { apiConfig = apiConfig
            , step = step
            }
    in
    stepFromRoute apiConfig navigationKey route
        |> Util.map setModel identity


setStep : Model -> Step -> Model
setStep model step =
    { model | step = step }


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        CreateProjectMsg subMsg ->
            case model.step of
                CreateProject subModel ->
                    CreateProject.update subMsg subModel
                        |> Util.map (setStep model << CreateProject) CreateProjectMsg

                _ ->
                    return model

        SlackIntegrationMsg subMsg ->
            case model.step of
                SlackIntegration subModel ->
                    SlackIntegration.update subMsg subModel
                        |> Util.map (setStep model << SlackIntegration) SlackIntegrationMsg

                _ ->
                    return model

        CreateEnvironmentsMsg subMsg ->
            case model.step of
                CreateEnvironments subModel ->
                    CreateEnvironments.update subMsg subModel
                        |> Util.map (setStep model << CreateEnvironments) CreateEnvironmentsMsg

                _ ->
                    return model

        CIIntegrationMsg subMsg ->
            case model.step of
                CIIntegration subModel ->
                    CIIntegration.update subMsg subModel
                        |> Util.map (setStep model << CIIntegration) CIIntegrationMsg

                _ ->
                    return model


form : Model -> Html Msg
form model =
    case model.step of
        CreateProject subModel ->
            CreateProject.view subModel
                |> Html.map CreateProjectMsg

        SlackIntegration subModel ->
            SlackIntegration.view subModel
                |> Html.map SlackIntegrationMsg

        CreateEnvironments subModel ->
            CreateEnvironments.view subModel
                |> Html.map CreateEnvironmentsMsg

        CIIntegration subModel ->
            CIIntegration.view subModel
                |> Html.map CIIntegrationMsg


stepTitle : Step -> String
stepTitle step =
    case step of
        CreateProject _ ->
            "Create project"

        SlackIntegration _ ->
            "Integrate with Slack"

        CreateEnvironments _ ->
            "Create environments"

        CIIntegration _ ->
            "Integrate with your CI"


steps : Model -> CheckmarkSteps.Steps
steps model =
    case model.step of
        CreateProject _ ->
            CheckmarkSteps.Steps
                []
                { text = "Create project" }
                [ { text = "Integrate with Slack" }
                , { text = "Create environments" }
                , { text = "Integrate with your CI" }
                ]

        SlackIntegration _ ->
            CheckmarkSteps.Steps
                [ { text = "Create project" } ]
                { text = "Integrate with Slack" }
                [ { text = "Create environments" }
                , { text = "Integrate with your CI" }
                ]

        CreateEnvironments _ ->
            CheckmarkSteps.Steps
                [ { text = "Create project" }
                , { text = "Integrate with Slack" }
                ]
                { text = "Create environments" }
                [ { text = "Integrate with your CI" } ]

        CIIntegration _ ->
            CheckmarkSteps.Steps
                [ { text = "Create project" }
                , { text = "Integrate with Slack" }
                , { text = "Create environments" }
                ]
                { text = "Integrate with your CI" }
                []


view : Model -> Html Msg
view model =
    AppHtml.container
        [ AppHtml.pageHeader (stepTitle model.step)
        , div [ class "row" ]
            [ div [ class "col col-md-8" ]
                [ form model ]
            , div [ class "col col-md-4 pt-6" ]
                [ CheckmarkSteps.view (steps model)
                ]
            ]
        ]

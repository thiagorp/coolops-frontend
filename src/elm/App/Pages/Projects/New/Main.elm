module App.Pages.Projects.New.Main exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

import App.Html as AppHtml
import App.Pages.Projects.New.CIIntegration.Main as CIIntegration
import App.Pages.Projects.New.CreateEnvironments.Main as CreateEnvironments
import App.Pages.Projects.New.CreateProject.Main as CreateProject
import App.Pages.Projects.New.SlackIntegration.Callback as SlackIntegrationCallback
import App.Pages.Projects.New.SlackIntegration.Main as SlackIntegration
import Components.CheckmarkSteps as CheckmarkSteps
import Html exposing (..)
import Html.Attributes exposing (..)
import Route
import Util exposing (PageHandler, andPerform, noop, return)


type Msg
    = CreateProjectMsg CreateProject.Msg
    | SlackIntegrationMsg SlackIntegration.Msg
    | SlackIntegrationCallbackMsg SlackIntegrationCallback.Msg
    | CreateEnvironmentsMsg CreateEnvironments.Msg
    | CIIntegrationMsg CIIntegration.Msg


type Step
    = CreateProject CreateProject.Model
    | SlackIntegration SlackIntegration.Model
    | SlackIntegrationCallback SlackIntegrationCallback.Model
    | CreateEnvironments CreateEnvironments.Model
    | CIIntegration CIIntegration.Model


type alias Model =
    { apiToken : String
    , baseUrl : String
    , step : Step
    }


stepFromRoute : String -> String -> Route.NavigationKey -> Route.NewProjectStep -> PageHandler Step Msg
stepFromRoute baseUrl apiToken navigationKey route =
    case route of
        Route.CreateProject ->
            CreateProject.init baseUrl apiToken navigationKey
                |> Util.map CreateProject CreateProjectMsg

        Route.IntegrateWithSlack projectId error ->
            SlackIntegration.init baseUrl apiToken navigationKey projectId error
                |> Util.map SlackIntegration SlackIntegrationMsg

        Route.IntegrateWithSlackCallback projectId code ->
            SlackIntegrationCallback.init baseUrl apiToken navigationKey code projectId
                |> Util.map SlackIntegrationCallback SlackIntegrationCallbackMsg

        Route.CreateEnvironments projectId ->
            CreateEnvironments.init baseUrl apiToken navigationKey projectId
                |> Util.map CreateEnvironments CreateEnvironmentsMsg

        Route.IntegrateWithCI projectId ->
            CIIntegration.init baseUrl apiToken navigationKey projectId
                |> Util.map CIIntegration CIIntegrationMsg


init : String -> String -> Route.NavigationKey -> Route.NewProjectStep -> PageHandler Model Msg
init baseUrl apiToken navigationKey route =
    let
        setModel step =
            { apiToken = apiToken
            , baseUrl = baseUrl
            , step = step
            }
    in
    stepFromRoute baseUrl apiToken navigationKey route
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

        SlackIntegrationCallbackMsg subMsg ->
            case model.step of
                SlackIntegrationCallback subModel ->
                    SlackIntegrationCallback.update subMsg subModel
                        |> Util.map (setStep model << SlackIntegrationCallback) SlackIntegrationCallbackMsg

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

        SlackIntegrationCallback subModel ->
            SlackIntegrationCallback.view subModel
                |> Html.map SlackIntegrationCallbackMsg

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

        SlackIntegrationCallback _ ->
            "Integrating with Slack..."

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

        SlackIntegrationCallback _ ->
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

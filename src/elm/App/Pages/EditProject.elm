module App.Pages.EditProject exposing (..)

import App.Api.EditProject as Api
import App.Api.GetProject exposing (Project)
import App.Api.GetSlackProjectIntegration exposing (..)
import App.Html as AppHtml
import App.Html.Form as Form
import Form.Validation as Validation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import RemoteData exposing (RemoteData(..), WebData)
import Route
import Util exposing (PageHandler, andPerform, noop, return)


type alias Model =
    { id : String
    , name : String
    , deploymentImage : String
    , accessToken : String
    , apiToken : String
    , formState : Validation.FormState Field
    , baseUrl : String
    , slackConfig : WebData SlackProjectIntegration
    }


init : String -> String -> Project -> PageHandler Model Msg
init baseUrl apiToken { id, deploymentImage, name, accessToken } =
    return
        { id = id
        , name = name
        , deploymentImage = deploymentImage
        , accessToken = accessToken
        , apiToken = apiToken
        , formState = Validation.initialState
        , baseUrl = baseUrl
        , slackConfig = Loading
        }
        |> andPerform
            (getSlackProjectIntegration baseUrl apiToken id SlackConfigResponse)


type Field
    = NameField
    | DeploymentImageField


type Msg
    = FieldUpdated Field String
    | SlackConfigResponse (Result Http.Error SlackProjectIntegration)
    | Submit
    | SubmitResponse (Result Http.Error ())


updateField : Model -> Field -> String -> Model
updateField model field value =
    case field of
        NameField ->
            { model | name = value }

        DeploymentImageField ->
            { model | deploymentImage = value }


formConfig : Validation.FormConfig Model Field (PageHandler Model Msg)
formConfig =
    let
        validator =
            Validation.all
                [ Validation.ifBlank .name NameField
                , Validation.ifBlank .deploymentImage DeploymentImageField
                ]
    in
    { validator = validator
    , successCallback = submit
    , errorCallback = noop
    }


submit : Model -> PageHandler Model Msg
submit model =
    return model
        |> andPerform (Api.editProject model.baseUrl model.apiToken model.id SubmitResponse model)


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        FieldUpdated field value ->
            updateField model field value
                |> Validation.validate formConfig
                |> return

        SlackConfigResponse result ->
            { model | slackConfig = RemoteData.fromResult result }
                |> return

        Submit ->
            Validation.submit formConfig model

        SubmitResponse (Err _) ->
            Validation.serverError model
                |> return

        SubmitResponse (Ok response) ->
            return model
                |> andPerform (Route.redirectTo (Route.Protected Route.ProjectsList))


redirectUri : SlackProjectIntegration -> String -> String
redirectUri { clientId } projectId =
    "https://slack.com/oauth/authorize?client_id=" ++ clientId ++ "&scope=chat:write,commands&single_channel=true&state=" ++ projectId


formBody : Model -> Bool -> List (Html Msg)
formBody model submitting =
    let
        onInput =
            Form.OnInput << FieldUpdated

        errorsOf =
            Validation.errorsOf model.formState

        submitting =
            Validation.isSubmitting model.formState

        inputs =
            [ Form.StaticTextInput
                { label = "Access token"
                , val = model.accessToken
                }
            , Form.TextInput
                { label = "Name"
                , placeholder = "Enter your project's name"
                , errors = errorsOf NameField
                , disabled = submitting
                , attributes = [ onInput NameField, Form.InputValue model.name ]
                , id = "new-project-form-name-input"
                }
            , Form.TextInput
                { label = "Deployment image"
                , placeholder = "Enter the docker image of your deployment"
                , errors = errorsOf DeploymentImageField
                , disabled = submitting
                , attributes = [ onInput DeploymentImageField, Form.InputValue model.deploymentImage ]
                , id = "new-project-form-deployment-image-input"
                }
            ]
    in
    List.map Form.input inputs
        ++ slackProjectIntegrationInput model


slackProjectIntegrationInput : Model -> List (Html Msg)
slackProjectIntegrationInput { slackConfig, id } =
    case slackConfig of
        NotAsked ->
            []

        Loading ->
            []

        Failure _ ->
            []

        Success config ->
            [ div [ class "form-group" ]
                [ label [ class "form-label" ] [ text "Slack synchronization" ]
                , AppHtml.externalLink
                    (redirectUri config id)
                    []
                    [ text "Sync" ]
                ]
            ]


form_ : Model -> Html Msg
form_ model =
    let
        submitting =
            Validation.isSubmitting model.formState
    in
    Html.form [ class "card", onSubmit Submit ]
        [ div [ class "card-body" ] (formBody model submitting)
        , div [ class "card-footer text-right" ]
            [ button
                [ classList [ ( "btn btn-primary", True ), ( "btn-loading", submitting ) ], disabled submitting ]
                [ text "Save" ]
            ]
        ]


view : Model -> Html Msg
view model =
    AppHtml.container
        [ AppHtml.pageHeader ("Edit " ++ model.name)
        , form_ model
        ]

module App.Pages.Projects.Edit.Form exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import App.Api.EditProject as Api
import App.Html as AppHtml
import App.Html.Form as Form
import App.Pages.Projects.Edit.Data as Data
import Form.Validation as Validation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Route
import Util exposing (PageHandler, andPerform, noop, return)


type alias Model =
    { apiToken : String
    , id : String
    , name : String
    , deploymentImage : String
    , accessToken : String
    , slackClientId : String
    , formState : Validation.FormState Field
    , baseUrl : String
    , navigationKey : Route.NavigationKey
    }


init : String -> String -> Route.NavigationKey -> Data.Project -> Data.SlackConfiguration -> PageHandler Model Msg
init baseUrl apiToken navigationKey { id, name, deploymentImage, accessToken } { clientId } =
    return
        { apiToken = apiToken
        , baseUrl = baseUrl
        , formState = Validation.initialState
        , id = id
        , name = name
        , deploymentImage = deploymentImage
        , accessToken = accessToken
        , slackClientId = clientId
        , navigationKey = navigationKey
        }


type Field
    = NameField
    | DeploymentImageField


type Msg
    = FieldUpdated Field String
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

        Submit ->
            Validation.submit formConfig model

        SubmitResponse (Err _) ->
            Validation.serverError model
                |> return

        SubmitResponse (Ok response) ->
            return model
                |> andPerform (Route.redirectTo model.navigationKey (Route.Protected Route.ProjectsList))


redirectUri : Model -> String
redirectUri { slackClientId, id } =
    "https://slack.com/oauth/authorize?client_id=" ++ slackClientId ++ "&scope=chat:write,commands&single_channel=true&state=" ++ id


formBody : Model -> Bool -> List (Html Msg)
formBody model submitting =
    let
        onInput =
            Form.OnInput << FieldUpdated

        errorsOf =
            Validation.errorsOf model.formState

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
                , hint = Nothing
                }
            , Form.TextInput
                { label = "Deployment image"
                , placeholder = "Enter the docker image of your deployment"
                , errors = errorsOf DeploymentImageField
                , disabled = submitting
                , attributes = [ onInput DeploymentImageField, Form.InputValue model.deploymentImage ]
                , id = "new-project-form-deployment-image-input"
                , hint = Nothing
                }
            ]
    in
    List.map Form.input inputs
        ++ slackProjectIntegrationInput model


slackProjectIntegrationInput : Model -> List (Html Msg)
slackProjectIntegrationInput model =
    [ div [ class "form-group" ]
        [ label [ class "form-label" ] [ text "Slack synchronization" ]
        , AppHtml.externalLink
            (redirectUri model)
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

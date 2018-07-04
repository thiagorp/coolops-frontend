module App.Pages.EditProject exposing (..)

import App.Api.EditProject as Api
import App.Api.GetProject exposing (Project)
import App.Html exposing (..)
import App.Html.Form as Form
import Form.Validation as Validation
import Html exposing (Html)
import Http
import Route
import Util exposing (PageHandler, andPerform, noop, return)


type alias Model =
    { id : String
    , name : String
    , deploymentImage : String
    , accessToken : String
    , apiToken : String
    , formState : Validation.FormState Field
    }


init : String -> Project -> PageHandler Model Msg
init apiToken { id, deploymentImage, name, accessToken } =
    return (Model id name deploymentImage accessToken apiToken Validation.initialState)


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
        |> andPerform (Api.editProject model.apiToken model.id SubmitResponse model)


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
                |> andPerform (Route.redirectTo (Route.Protected Route.ProjectsList))


form : Model -> Html Msg
form { name, deploymentImage, accessToken, formState } =
    let
        onInput =
            Form.OnInput << FieldUpdated

        errorsOf =
            Validation.errorsOf formState

        submitting =
            Validation.isSubmitting formState

        inputs =
            [ Form.StaticTextInput
                { label = "Access token"
                , val = accessToken
                }
            , Form.TextInput
                { label = "Name"
                , placeholder = "Enter your project's name"
                , errors = errorsOf NameField
                , disabled = submitting
                , attributes = [ onInput NameField, Form.InputValue name ]
                , id = "new-project-form-name-input"
                }
            , Form.TextInput
                { label = "Deployment image"
                , placeholder = "Enter the docker image of your deployment"
                , errors = errorsOf DeploymentImageField
                , disabled = submitting
                , attributes = [ onInput DeploymentImageField, Form.InputValue deploymentImage ]
                , id = "new-project-form-deployment-image-input"
                }
            ]

        formConfig =
            { loading = submitting
            , error = Validation.getServerError formState
            , submitButtonText = "Create"
            , msg = Submit
            }
    in
    Form.linearCardForm formConfig inputs


view : Model -> Html Msg
view model =
    container
        [ pageHeader ("Edit " ++ model.name)
        , form model
        ]

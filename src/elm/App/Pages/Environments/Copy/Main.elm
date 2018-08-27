module App.Pages.Environments.Copy.Main exposing (Environment, Field(..), Model, Msg(..), form, formConfig, init, submit, update, view)

import Api exposing (ApiData, ApiResult)
import App.Api.CreateEnvironment as Api
import App.Html.Form as Form
import App.Pages.Environments.Copy.Data exposing (..)
import Dict exposing (Dict)
import Form.Validation as Validation
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import RemoteData exposing (RemoteData(..))
import Route
import SelectList
import Util exposing (PageHandler, andPerform, noop, return)


type alias Model =
    { name : String
    , projectId : String
    , environmentVars : Dict String String
    , editingKey : String
    , editingValue : String
    , editingKeyError : Maybe (List String)
    , apiToken : String
    , formState : Validation.FormState Field
    , baseUrl : String
    , projects : ApiData (List Project)
    }


type Msg
    = NameUpdated String
    | ProjectUpdated String
    | EnvVarValueUpdated String
    | EnvVarKeyUpdated String
    | EnvVarAdded
    | EnvVarRemoved String
    | EnvVarEditClicked String
    | ProjectsResponse (ApiResult (List Project))
    | Submit
    | SubmitResponse (Result Http.Error ())


type Field
    = NameField
    | ProjectField


type alias Environment a =
    { a | environmentVars : Dict String String, projectId : String }


init : String -> String -> Environment a -> PageHandler Model Msg
init baseUrl apiToken { environmentVars, projectId } =
    return
        { name = ""
        , projectId = projectId
        , environmentVars = environmentVars
        , editingKey = ""
        , editingValue = ""
        , editingKeyError = Nothing
        , apiToken = apiToken
        , formState = Validation.initialState
        , baseUrl = baseUrl
        , projects = Loading
        }
        |> andPerform (listProjects baseUrl apiToken ProjectsResponse)


formConfig : Validation.FormConfig Model Field (PageHandler Model Msg)
formConfig =
    let
        validator =
            Validation.all
                [ Validation.ifBlank .name NameField
                , Validation.ifBlank .projectId ProjectField
                ]
    in
    { validator = validator
    , successCallback = submit
    , errorCallback = noop
    }


submit : Model -> PageHandler Model Msg
submit model =
    return model
        |> andPerform (Api.createEnvironment model.baseUrl model.apiToken SubmitResponse model)


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        NameUpdated newName ->
            { model | name = newName }
                |> Validation.validate formConfig
                |> return

        ProjectUpdated newProject ->
            { model | projectId = newProject }
                |> Validation.validate formConfig
                |> return

        EnvVarKeyUpdated newKey ->
            { model | editingKey = newKey, editingKeyError = Nothing }
                |> return

        EnvVarValueUpdated newValue ->
            { model | editingValue = newValue }
                |> return

        EnvVarAdded ->
            case model.editingKey of
                "" ->
                    { model | editingKeyError = Just [ "The key can't be blank" ] }
                        |> return

                _ ->
                    { model
                        | environmentVars = Dict.insert model.editingKey model.editingValue model.environmentVars
                        , editingKey = ""
                        , editingValue = ""
                    }
                        |> return

        EnvVarEditClicked key ->
            case Dict.get key model.environmentVars of
                Nothing ->
                    return model

                Just val ->
                    { model
                        | environmentVars = Dict.remove key model.environmentVars
                        , editingKey = key
                        , editingValue = val
                        , editingKeyError = Nothing
                    }
                        |> return

        EnvVarRemoved key ->
            { model | environmentVars = Dict.remove key model.environmentVars }
                |> return

        ProjectsResponse result ->
            { model | projects = RemoteData.fromResult result }
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
form model =
    let
        submitting =
            Validation.isSubmitting model.formState

        projectsList =
            case model.projects of
                Success projects ->
                    projects
                        |> List.map (\{ id, name } -> { key = id, val = name })
                        |> SelectList.fromLists [] { key = "", val = "Select a project" }
                        |> SelectList.select (\{ key } -> key == model.projectId)

                _ ->
                    SelectList.singleton { key = "", val = "Select a project" }

        inputs =
            [ Form.DropdownInput
                { label = "Project"
                , options = projectsList
                , errors = Validation.errorsOf model.formState ProjectField
                , disabled = submitting || RemoteData.isLoading model.projects
                , id = "new-environment-form-project-input"
                , events =
                    { onSelect = ProjectUpdated }
                }
            , Form.TextInput
                { label = "Name"
                , placeholder = "Enter your environment's name"
                , errors = Validation.errorsOf model.formState NameField
                , disabled = submitting
                , attributes = [ Form.OnInput NameUpdated, Form.InputValue model.name ]
                , id = "new-environment-form-name-input"
                }
            , Form.KeyValueInput
                { label = "Environment variables"
                , disabled = submitting
                , placeholder = { key = "Key", val = "Value" }
                , errors = { key = model.editingKeyError, val = Nothing }
                , id = "new-environment-form-env-vars-input"
                , events =
                    { onKeyChange = EnvVarKeyUpdated
                    , onValueChange = EnvVarValueUpdated
                    , onEntryRemove = EnvVarRemoved
                    , onEntryAdd = EnvVarAdded
                    , onEntryEdit = EnvVarEditClicked
                    }
                , values =
                    { added = model.environmentVars
                    , editing = { key = model.editingKey, val = model.editingValue }
                    }
                }
            ]

        formConfig =
            { loading = submitting
            , error = Validation.getServerError model.formState
            , submitButtonText = "Create"
            , msg = Submit
            }
    in
    Form.linearCardForm formConfig inputs


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "page-header" ]
            [ h1 [ class "page-title" ] [ text "Create environment" ] ]
        , form model
        ]

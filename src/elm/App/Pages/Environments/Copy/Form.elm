module App.Pages.Environments.Copy.Form exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import App.Api.CreateEnvironment as Api
import App.Html.Form as Form
import App.Pages.Environments.Copy.Data as Data
import Dict exposing (Dict)
import Form.Validation as Validation
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Route
import SelectList
import Util exposing (PageHandler, andPerform, noop, return)
import Util.Slug as Slug


type alias Model =
    { name : String
    , slug : String
    , slugModified : Bool
    , projectId : String
    , environmentVars : Dict String String
    , editingKey : String
    , editingValue : String
    , editingKeyError : Maybe (List String)
    , apiToken : String
    , formState : Validation.FormState Field
    , baseUrl : String
    , projects : List Data.Project
    }


type Msg
    = NameUpdated String
    | SlugUpdated String
    | ProjectUpdated String
    | EnvVarValueUpdated String
    | EnvVarKeyUpdated String
    | EnvVarAdded
    | EnvVarRemoved String
    | EnvVarEditClicked String
    | Submit
    | SubmitResponse (Result Http.Error ())


type Field
    = NameField
    | ProjectField
    | SlugField


init : String -> String -> Data.Environment -> List Data.Project -> PageHandler Model Msg
init baseUrl apiToken { environmentVars, projectId } projects =
    return
        { name = ""
        , slug = ""
        , slugModified = False
        , projectId = projectId
        , environmentVars = environmentVars
        , editingKey = ""
        , editingValue = ""
        , editingKeyError = Nothing
        , apiToken = apiToken
        , formState = Validation.initialState
        , baseUrl = baseUrl
        , projects = projects
        }


formConfig : Validation.FormConfig Model Field (PageHandler Model Msg)
formConfig =
    let
        validator =
            Validation.all
                [ Validation.ifBlank .name NameField
                , Validation.ifBlank .projectId ProjectField
                , Validation.ifBlank .slug SlugField
                , Validation.ifInvalidSlug .slug SlugField
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
            let
                newSlug =
                    case model.slugModified of
                        True ->
                            model.slug

                        False ->
                            Slug.slugify newName
            in
            { model | name = newName, slug = newSlug }
                |> Validation.validate formConfig
                |> return

        SlugUpdated newSlug ->
            { model | slug = newSlug, slugModified = True }
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
            model.projects
                |> List.map (\{ id, name } -> { key = id, val = name })
                |> SelectList.fromLists [] { key = "", val = "Select a project" }
                |> SelectList.select (\{ key } -> key == model.projectId)

        inputs =
            [ Form.DropdownInput
                { label = "Project"
                , options = projectsList
                , errors = Validation.errorsOf model.formState ProjectField
                , disabled = submitting
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
                , hint = Nothing
                }
            , Form.TextInput
                { label = "Slug"
                , placeholder = "Enter your Environment's slug"
                , errors = Validation.errorsOf model.formState SlugField
                , disabled = submitting
                , attributes = [ Form.InputValue model.slug, Form.OnInput SlugUpdated ]
                , id = "new-environment-form-slug-input"
                , hint = Just [ text "You will use this to refer to your environment on Slack" ]
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

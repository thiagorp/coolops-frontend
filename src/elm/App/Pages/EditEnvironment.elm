module App.Pages.EditEnvironment exposing (..)

import App.Api.GetEnvironment exposing (Environment)
import App.Api.UpdateEnvironment as Api
import App.Html.Form as Form
import Dict exposing (Dict)
import Form.Validation as Validation
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Route
import Util exposing (PageHandler, andPerform, noop, return)


type alias Model =
    { id : String
    , name : String
    , environmentVars : Dict String String
    , editingKey : String
    , editingValue : String
    , editingKeyError : Maybe (List String)
    , apiToken : String
    , formState : Validation.FormState Field
    }


type Msg
    = NameUpdated String
    | EnvVarValueUpdated String
    | EnvVarKeyUpdated String
    | EnvVarAdded
    | EnvVarRemoved String
    | EnvVarEditClicked String
    | Submit
    | SubmitResponse (Result Http.Error ())


type Field
    = NameField


init : String -> Environment -> PageHandler Model Msg
init apiToken { id, name, environmentVars } =
    return (Model id name environmentVars "" "" Nothing apiToken Validation.initialState)


formConfig : Validation.FormConfig Model Field (PageHandler Model Msg)
formConfig =
    let
        validator =
            Validation.all
                [ Validation.ifBlank .name NameField
                ]
    in
    { validator = validator
    , successCallback = submit
    , errorCallback = noop
    }


submit : Model -> PageHandler Model Msg
submit model =
    return model
        |> andPerform (Api.updateEnvironment model.apiToken model.id SubmitResponse model)


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        NameUpdated newName ->
            { model | name = newName }
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

        inputs =
            [ Form.TextInput
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
            [ h1 [ class "page-title" ] [ text ("Edit " ++ model.name) ] ]
        , form model
        ]

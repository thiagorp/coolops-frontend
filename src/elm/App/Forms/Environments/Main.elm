module App.Forms.Environments.Main exposing
    ( Model
    , Msg(..)
    , init
    , isSubmitting
    , update
    , view
    )

import App.Api.CreateEnvironment as Api
import App.Forms.Environments.Data as Data
import App.Html.Form as Form
import Dict
import Form.Validation as Validation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Route
import Util exposing (PageHandler, andPerform, noop, return)
import Util.Slug as Slug


type alias Model =
    { name : String
    , slug : String
    , slugModified : Bool
    , environmentVars : Dict.Dict String String
    , editingKey : String
    , editingValue : String
    , editingKeyError : Maybe (List String)
    , projectId : String
    , apiToken : String
    , formState : Validation.FormState Field
    , baseUrl : String
    , projects : List Data.Project
    , selectedEnvToCopy : Maybe Data.Environment
    }


type Msg
    = CopyEnvVars
    | EnvToCopyFromSelected String
    | EnvVarValueUpdated String
    | EnvVarKeyUpdated String
    | EnvVarAdded
    | EnvVarRemoved String
    | EnvVarEditClicked String
    | NameUpdated String
    | SlugUpdated String
    | Submit
    | SubmitResponse (Result Http.Error ())


type Field
    = NameField
    | SlugField



-- Init


init : String -> String -> String -> List Data.Project -> PageHandler Model Msg
init baseUrl apiToken projectId projects =
    return
        { name = ""
        , slug = ""
        , slugModified = False
        , environmentVars = Dict.empty
        , editingKey = ""
        , editingValue = ""
        , editingKeyError = Nothing
        , projectId = projectId
        , apiToken = apiToken
        , formState = Validation.initialState
        , baseUrl = baseUrl
        , projects = projects
        , selectedEnvToCopy = Nothing
        }



-- Update


formConfig : Validation.FormConfig Model Field (PageHandler Model Msg)
formConfig =
    let
        validator =
            Validation.all
                [ Validation.ifBlank .name NameField
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

        CopyEnvVars ->
            case model.selectedEnvToCopy of
                Nothing ->
                    return model

                Just env ->
                    return
                        { model
                            | selectedEnvToCopy = Nothing
                            , environmentVars = Dict.union model.environmentVars env.environmentVars
                        }

        EnvToCopyFromSelected envId ->
            let
                selectedEnv =
                    model.projects
                        |> List.map .environments
                        |> List.foldr (++) []
                        |> List.filter (\e -> e.id == envId)
                        |> List.head
            in
            return { model | selectedEnvToCopy = selectedEnv }

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

        SubmitResponse (Err (Http.BadStatus response)) ->
            case response.status.code of
                409 ->
                    model
                        |> Validation.addError SlugField "This slug already exists on this project"
                        |> return

                _ ->
                    Validation.serverError model
                        |> return

        SubmitResponse (Err _) ->
            Validation.serverError model
                |> return

        SubmitResponse (Ok response) ->
            return model


isSubmitting : Model -> Bool
isSubmitting model =
    Validation.isSubmitting model.formState



-- View


projectOptgroup : Data.Project -> Html Msg
projectOptgroup { name, environments } =
    optgroup [ attribute "label" name ]
        (List.map (\e -> option [ value e.id ] [ text e.name ]) environments)


copyEnvironment : Model -> Html Msg
copyEnvironment model =
    let
        noEnvSelected =
            case model.selectedEnvToCopy of
                Nothing ->
                    True

                Just _ ->
                    False
    in
    case List.foldr ((+) << List.length << .environments) 0 model.projects of
        0 ->
            div [] []

        _ ->
            div [ class "form-group" ]
                [ label
                    [ class "form-label"
                    , for "new-environment-form-copy-environment-vars-input"
                    ]
                    [ text "Copy environment variables from another Environment" ]
                , div [ class "row gutter-xs" ]
                    [ div [ class "col" ]
                        [ select
                            [ id "new-environment-form-copy-environment-vars-input"
                            , class "form-control"
                            , onInput EnvToCopyFromSelected
                            , disabled (isSubmitting model)
                            ]
                            ([ option [ selected noEnvSelected ] [ text "" ] ]
                                ++ List.map projectOptgroup model.projects
                            )
                        ]
                    , div [ class "col-auto" ]
                        [ button
                            [ type_ "button"
                            , class "btn btn-secondary"
                            , onClick CopyEnvVars
                            , disabled (noEnvSelected || isSubmitting model)
                            ]
                            [ text "Copy" ]
                        ]
                    ]
                , div
                    []
                    [ small
                        [ class "form-text text-muted" ]
                        [ text "You will be able to edit them after you copy" ]
                    ]
                ]


view : Model -> Html Msg
view model =
    let
        nameInput =
            Form.TextInput
                { label = "Name"
                , placeholder = "E.g. Production, Staging"
                , errors = Validation.errorsOf model.formState NameField
                , disabled = isSubmitting model
                , attributes = [ Form.OnInput NameUpdated ]
                , id = "new-environment-form-name-input"
                , hint = Nothing
                }

        slugInput =
            Form.TextInput
                { label = "Slug"
                , placeholder = "Enter your Environment's slug"
                , errors = Validation.errorsOf model.formState SlugField
                , disabled = isSubmitting model
                , attributes = [ Form.InputValue model.slug, Form.OnInput SlugUpdated ]
                , id = "new-environment-form-slug-input"
                , hint = Just [ text "It's unique inside the project. You will use it to refer to your environment on Slack" ]
                }

        envVarsInput =
            Form.KeyValueInput
                { label = "Environment variables"
                , disabled = isSubmitting model
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

        error =
            case Validation.getServerError model.formState of
                Nothing ->
                    []

                Just e ->
                    [ p [ class "text-red" ] [ text e ] ]
    in
    div []
        ([ Form.input nameInput
         , Form.input slugInput
         , copyEnvironment model
         , Form.input envVarsInput
         ]
            ++ error
        )

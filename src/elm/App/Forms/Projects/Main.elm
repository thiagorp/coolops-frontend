module App.Forms.Projects.Main exposing
    ( CreateOrUpdate(..)
    , Model
    , Msg(..)
    , init
    , isSubmitting
    , update
    , view
    )

import Api
import App.Api.CreateProject as Api
import App.Api.EditProject as Api
import App.Html.Form as Form
import Form.Validation as Validation
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Route
import Util exposing (PageHandler, andPerform, noop, return)
import Util.Slug as Slug


type Action
    = ActionCreate
    | ActionUpdate String


type alias Model =
    { name : String
    , slug : String
    , slugModified : Bool
    , deploymentImage : String
    , apiConfig : Api.ProtectedConfig
    , formState : Validation.FormState Field
    , action : Action
    }


type Field
    = NameField
    | DeploymentImageField
    | SlugField


type Msg
    = FieldUpdated Field String
    | Submit
    | SubmitResponse (Result Http.Error Api.Project)



-- Init


type alias Project a =
    { a
        | id : String
        , name : String
        , slug : String
        , deploymentImage : String
    }


type CreateOrUpdate a
    = Create
    | Update (Project a)


init : Api.ProtectedConfig -> CreateOrUpdate a -> PageHandler Model Msg
init apiConfig actionData =
    let
        initial =
            case actionData of
                Create ->
                    { name = ""
                    , slug = ""
                    , deploymentImage = ""
                    , action = ActionCreate
                    }

                Update p ->
                    { name = p.name
                    , slug = p.slug
                    , deploymentImage = p.deploymentImage
                    , action = ActionUpdate p.id
                    }
    in
    return
        { name = initial.name
        , slug = initial.slug
        , slugModified = False
        , deploymentImage = initial.deploymentImage
        , action = initial.action
        , apiConfig = apiConfig
        , formState = Validation.initialState
        }


updateField : Model -> Field -> String -> Model
updateField model field value =
    case field of
        NameField ->
            let
                newSlug =
                    case model.slugModified of
                        True ->
                            model.slug

                        False ->
                            Slug.slugify value
            in
            { model | name = value, slug = newSlug }

        SlugField ->
            { model | slug = value, slugModified = True }

        DeploymentImageField ->
            { model | deploymentImage = value }


formConfig : Validation.FormConfig Model Field (PageHandler Model Msg)
formConfig =
    let
        validator =
            Validation.all
                [ Validation.ifBlank .name NameField
                , Validation.ifBlank .slug SlugField
                , Validation.ifInvalidSlug .slug SlugField
                , Validation.ifBlank .deploymentImage DeploymentImageField
                ]
    in
    { validator = validator
    , successCallback = submit
    , errorCallback = noop
    }


submit : Model -> PageHandler Model Msg
submit model =
    case model.action of
        ActionCreate ->
            return model
                |> andPerform (Api.createProject model.apiConfig SubmitResponse model)

        ActionUpdate projectId ->
            return model
                |> andPerform (Api.editProject model.apiConfig projectId SubmitResponse model)


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        FieldUpdated field value ->
            updateField model field value
                |> Validation.validate formConfig
                |> return

        Submit ->
            Validation.submit formConfig model

        SubmitResponse (Err (Http.BadStatus response)) ->
            case response.status.code of
                409 ->
                    model
                        |> Validation.addError SlugField "Slug already exists"
                        |> return

                _ ->
                    Validation.serverError model
                        |> return

        SubmitResponse (Err _) ->
            Validation.serverError model
                |> return

        SubmitResponse (Ok response) ->
            return { model | formState = Validation.initialState }


isSubmitting : Model -> Bool
isSubmitting { formState } =
    Validation.isSubmitting formState


view : Model -> List (Html Msg)
view model =
    let
        onInput =
            Form.OnInput << FieldUpdated

        errorsOf =
            Validation.errorsOf model.formState

        inputs =
            [ Form.TextInput
                { label = "Name"
                , placeholder = "Enter your project's name"
                , errors = errorsOf NameField
                , disabled = isSubmitting model
                , attributes = [ onInput NameField, Form.InputValue model.name ]
                , id = "new-project-form-name-input"
                , hint = Nothing
                }
            , Form.TextInput
                { label = "Slug"
                , placeholder = "Enter your project's slug"
                , errors = errorsOf SlugField
                , disabled = isSubmitting model
                , attributes = [ onInput SlugField, Form.InputValue model.slug ]
                , id = "new-project-form-slug-input"
                , hint = Just [ text "You will use this to refer your project on Slack" ]
                }
            , Form.TextInput
                { label = "Deployment image"
                , placeholder = "E.g. coolopsio/kubernetes:latest"
                , errors = errorsOf DeploymentImageField
                , disabled = isSubmitting model
                , attributes = [ onInput DeploymentImageField, Form.InputValue model.deploymentImage ]
                , id = "new-project-form-deployment-image-input"
                , hint =
                    Just
                        [ a [ href "https://github.com/coolopsio/deployment-images", target "_blank" ] [ text "Click here" ]
                        , text " if you are not sure what this is"
                        ]
                }
            ]

        error =
            case Validation.getServerError model.formState of
                Nothing ->
                    []

                Just e ->
                    [ p [ class "text-red" ] [ text e ] ]
    in
    List.map Form.input inputs ++ error

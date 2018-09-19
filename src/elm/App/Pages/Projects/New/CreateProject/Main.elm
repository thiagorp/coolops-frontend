module App.Pages.Projects.New.CreateProject.Main exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import App.Api.CreateProject as Api
import App.Html.Form as Form
import Form.Validation as Validation
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Route
import Util exposing (PageHandler, andPerform, noop, return)
import Util.Slug as Slug


type alias Model =
    { name : String
    , slug : String
    , slugModified : Bool
    , deploymentImage : String
    , apiToken : String
    , formState : Validation.FormState Field
    , baseUrl : String
    , navigationKey : Route.NavigationKey
    }


type Field
    = NameField
    | DeploymentImageField
    | SlugField


type Msg
    = FieldUpdated Field String
    | Submit
    | SubmitResponse (Result Http.Error Api.Project)


init : String -> String -> Route.NavigationKey -> PageHandler Model Msg
init baseUrl apiToken navigationKey =
    return
        { name = ""
        , slug = ""
        , slugModified = False
        , deploymentImage = ""
        , apiToken = apiToken
        , formState = Validation.initialState
        , baseUrl = baseUrl
        , navigationKey = navigationKey
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
    return model
        |> andPerform (Api.createProject model.baseUrl model.apiToken SubmitResponse model)


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
            let
                redirect =
                    Route.IntegrateWithSlack response.id False
                        |> Route.NewProject
                        |> Route.Protected
                        |> Route.redirectTo model.navigationKey
            in
            return model
                |> andPerform redirect


view : Model -> Html Msg
view { formState, slug } =
    let
        onInput =
            Form.OnInput << FieldUpdated

        errorsOf =
            Validation.errorsOf formState

        submitting =
            Validation.isSubmitting formState

        inputs =
            [ Form.TextInput
                { label = "Name"
                , placeholder = "Enter your project's name"
                , errors = errorsOf NameField
                , disabled = submitting
                , attributes = [ onInput NameField ]
                , id = "new-project-form-name-input"
                , hint = Nothing
                }
            , Form.TextInput
                { label = "Slug"
                , placeholder = "Enter your project's slug"
                , errors = errorsOf SlugField
                , disabled = submitting
                , attributes = [ onInput SlugField, Form.InputValue slug ]
                , id = "new-project-form-slug-input"
                , hint = Just [ text "You will use this to refer your project on Slack" ]
                }
            , Form.TextInput
                { label = "Deployment image"
                , placeholder = "E.g. coolopsio/kubernetes:latest"
                , errors = errorsOf DeploymentImageField
                , disabled = submitting
                , attributes = [ onInput DeploymentImageField ]
                , id = "new-project-form-deployment-image-input"
                , hint =
                    Just
                        [ a [ href "https://github.com/coolopsio/deployment-images", target "_blank" ] [ text "Click here" ]
                        , text " if you are not sure what this is"
                        ]
                }
            ]

        formData =
            { loading = submitting
            , error = Validation.getServerError formState
            , submitButtonText = "Continue"
            , msg = Submit
            }
    in
    Form.linearCardForm formData inputs

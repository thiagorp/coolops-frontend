module Auth.Pages.Signup exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Auth.Api as Api
import Auth.Layouts.Authentication as AuthenticationLayout
import Form.Html exposing (InputAttribute(..), InputType(..))
import Form.Validation as Validation
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, href)
import Http as Http
import Ports
import Route as Route exposing (AuthRoute(..), Route(..), toUrl)
import Util exposing (PageHandler, andPerform, noop, return)



---- MODEL ----


type alias Model =
    { firstName : String
    , lastName : String
    , email : String
    , password : String
    , companyName : String
    , formState : Validation.FormState Field
    , baseUrl : String
    }


init : String -> PageHandler Model Msg
init baseUrl =
    return
        { firstName = ""
        , lastName = ""
        , email = ""
        , password = ""
        , companyName = ""
        , formState = Validation.initialState
        , baseUrl = baseUrl
        }



---- UPDATE ----


type Field
    = FirstNameField
    | LastNameField
    | EmailField
    | CompanyNameField
    | PasswordField


type Msg
    = FieldUpdated Field String
    | Signup
    | SignupCallback (Result Http.Error Api.SignupResponse)


updateField : Model -> Field -> String -> Model
updateField model field value =
    case field of
        FirstNameField ->
            { model | firstName = value }

        LastNameField ->
            { model | lastName = value }

        EmailField ->
            { model | email = value }

        PasswordField ->
            { model | password = value }

        CompanyNameField ->
            { model | companyName = value }


formConfig : Validation.FormConfig Model Field (PageHandler Model Msg)
formConfig =
    let
        validator =
            Validation.all
                [ Validation.ifBlank .firstName FirstNameField
                , Validation.ifBlank .lastName LastNameField
                , Validation.ifBlank .email EmailField
                , Validation.ifInvalidEmail .email EmailField
                , Validation.ifBlank .password PasswordField
                , Validation.ifShorterThan 8 .password PasswordField
                , Validation.ifBlank .companyName CompanyNameField
                ]
    in
    { validator = validator
    , successCallback = signup
    , errorCallback = noop
    }


signup : Model -> PageHandler Model Msg
signup model =
    return model
        |> andPerform (Api.signup model.baseUrl model SignupCallback)


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        FieldUpdated field value ->
            updateField model field value
                |> Validation.validate formConfig
                |> return

        Signup ->
            Validation.submit formConfig model

        SignupCallback result ->
            case result of
                Err (Http.BadStatus response) ->
                    case response.status.code of
                        409 ->
                            model
                                |> Validation.addError EmailField "Email already exists"
                                |> return

                        _ ->
                            model
                                |> Validation.serverError
                                |> return

                Err error ->
                    model
                        |> Validation.serverError
                        |> return

                Ok response ->
                    return model
                        |> andPerform (Ports.login response.accessToken)



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        onInput =
            OnInput << FieldUpdated

        errorsOf =
            Validation.errorsOf model.formState

        submitting =
            Validation.isSubmitting model.formState

        inputs =
            [ { label = "First name"
              , placeholder = "Enter your first name"
              , errors = errorsOf FirstNameField
              , disabled = submitting
              , attributes = [ onInput FirstNameField ]
              }
            , { label = "Last name"
              , placeholder = "Enter your last name"
              , errors = errorsOf LastNameField
              , disabled = submitting
              , attributes = [ onInput LastNameField ]
              }
            , { label = "Email"
              , placeholder = "Enter your email"
              , errors = errorsOf EmailField
              , disabled = submitting
              , attributes = [ InputType Email, onInput EmailField ]
              }
            , { label = "Company name"
              , placeholder = "Enter your company's name"
              , errors = errorsOf CompanyNameField
              , disabled = submitting
              , attributes = [ onInput CompanyNameField ]
              }
            , { label = "Password"
              , placeholder = "Enter your password"
              , errors = errorsOf PasswordField
              , disabled = submitting
              , attributes = [ InputType Password, onInput PasswordField ]
              }
            ]

        serverError =
            Validation.getServerError model.formState
    in
    AuthenticationLayout.layout
        [ AuthenticationLayout.form submitting Signup "Create new account" serverError inputs
        , div [ class "text-center text-muted" ]
            [ text "Already have an account? "
            , a [ href (toUrl (Auth Login)) ] [ text "Sign in" ]
            ]
        ]

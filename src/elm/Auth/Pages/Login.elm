module Auth.Pages.Login exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Api
import Auth.Api as Api
import Auth.Layouts.Authentication as AuthenticationLayout
import Form.Html exposing (..)
import Form.Validation as Validation
import Html exposing (Html, div, text)
import Http as Http
import Ports
import Route exposing (AuthRoute(..), Route(..), toUrl)
import Util exposing (PageHandler, andPerform, noop, return)



---- MODEL ----


type alias Model =
    { email : String
    , password : String
    , formState : Validation.FormState Field
    , apiConfig : Api.PublicConfig
    }


init : Api.PublicConfig -> PageHandler Model Msg
init apiConfig =
    return
        { email = ""
        , password = ""
        , formState = Validation.initialState
        , apiConfig = apiConfig
        }



---- UPDATE ----


type Field
    = EmailField
    | PasswordField


type Msg
    = FieldUpdated Field String
    | Login
    | LoginCallback (Result Http.Error Api.SignupResponse)


updateField : Model -> Field -> String -> Model
updateField model field value =
    case field of
        EmailField ->
            { model | email = value }

        PasswordField ->
            { model | password = value }


formConfig : Validation.FormConfig Model Field (PageHandler Model Msg)
formConfig =
    { validator = Validation.all []
    , successCallback = login
    , errorCallback = noop
    }


login : Model -> PageHandler Model Msg
login model =
    return model
        |> andPerform (Api.login model.apiConfig model LoginCallback)


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        FieldUpdated field value ->
            updateField model field value
                |> Validation.validate formConfig
                |> return

        Login ->
            Validation.submit formConfig model

        LoginCallback result ->
            case result of
                Err (Http.BadStatus response) ->
                    case List.member response.status.code [ 401, 422 ] of
                        True ->
                            Validation.uncategorizedError "Invalid email or password" model
                                |> return

                        False ->
                            Validation.serverError model
                                |> return

                Err _ ->
                    Validation.serverError model
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
            [ { label = "Email"
              , placeholder = "Enter your email"
              , errors = errorsOf EmailField
              , disabled = submitting
              , attributes = [ InputType Email, onInput EmailField ]
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
        [ AuthenticationLayout.form submitting Login "Login" serverError inputs

        -- , div [ class "text-center text-muted" ]
        -- [ text "Don't have account yet? "
        -- , a (Auth Signup) LinkClicked [] [ text "Sign up" ]
        -- ]
        ]

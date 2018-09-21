module App.Pages.Projects.New.CreateEnvironments.Main exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Api
import App.Forms.Environments.Main as Form
import App.Html as AppHtml
import App.Pages.Projects.New.CreateEnvironments.Data as Data
import App.Pages.ServerError as ServerError
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route
import Util exposing (PageHandler, andPerform, noop, return)


type Remote
    = Loading
    | Error
    | Loaded Form.Model Data.Project


type alias Model =
    { apiToken : String
    , baseUrl : String
    , data : Remote
    , projectId : String
    , navigationKey : Route.NavigationKey
    }


type Msg
    = DataLoaded (Api.ApiResult Data.Response)
    | FormMsg Form.Msg


init : String -> String -> Route.NavigationKey -> String -> PageHandler Model Msg
init baseUrl apiToken navigationKey projectId =
    return
        { apiToken = apiToken
        , baseUrl = baseUrl
        , data = Loading
        , projectId = projectId
        , navigationKey = navigationKey
        }
        |> andPerform (Data.getData baseUrl apiToken projectId DataLoaded)


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        FormMsg subMsg ->
            case model.data of
                Loaded subModel project ->
                    case subMsg of
                        Form.SubmitResponse (Ok _) ->
                            return model
                                |> andPerform (Data.getData model.baseUrl model.apiToken model.projectId DataLoaded)

                        _ ->
                            Form.update subMsg subModel
                                |> Util.map (\f -> { model | data = Loaded f project }) FormMsg

                _ ->
                    return model

        DataLoaded (Ok response) ->
            case response.currentProject of
                Just project ->
                    let
                        updateModel subModel =
                            { model | data = Loaded subModel project }
                    in
                    Form.init model.baseUrl model.apiToken (Form.Create model.projectId) response.formData
                        |> Util.map updateModel FormMsg

                Nothing ->
                    return model
                        |> andPerform (Route.redirectTo model.navigationKey (Route.Protected (Route.NewProject Route.CreateProject)))

        DataLoaded (Err _) ->
            return { model | data = Error }


existingEnvironment : Data.Environment -> Html msg
existingEnvironment { name } =
    div [ class "card" ]
        [ div [ class "card-header" ]
            [ h3 [ class "card-title" ] [ text name ]
            ]
        ]


form : Form.Model -> Data.Project -> Html Msg
form subModel project =
    Html.form [ class "card", onSubmit Form.Submit ]
        [ div [ class "card-body" ] [ Form.view subModel ]
        , div [ class "card-footer text-right" ]
            [ a
                [ class "btn btn-link mr-2", href (Route.toUrl (Route.Protected (Route.NewProject (Route.IntegrateWithCI subModel.projectId)))) ]
                [ text "Continue to the next step (unsaved data will be lost)" ]
            , button [ class "btn btn-primary", type_ "submit", disabled (Form.isSubmitting subModel) ] [ text "Save" ]
            ]
        ]
        |> Html.map FormMsg


view : Model -> Html Msg
view model =
    case model.data of
        Loading ->
            div [ class "card" ]
                [ div [ class "card-body" ]
                    [ AppHtml.spinner ]
                ]

        Error ->
            ServerError.view

        Loaded f project ->
            div []
                (List.map existingEnvironment project.environments
                    ++ [ form f project ]
                )

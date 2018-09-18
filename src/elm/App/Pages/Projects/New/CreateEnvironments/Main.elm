module App.Pages.Projects.New.CreateEnvironments.Main exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Api
import App.Html as AppHtml
import App.Pages.Projects.New.CreateEnvironments.Data as Data
import App.Pages.Projects.New.CreateEnvironments.Form as Form
import App.Pages.ServerError as ServerError
import Html exposing (..)
import Html.Attributes exposing (..)
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
    }


type Msg
    = DataLoaded (Api.ApiResult Data.Response)
    | FormMsg Form.Msg


init : String -> String -> String -> PageHandler Model Msg
init baseUrl apiToken projectId =
    return
        { apiToken = apiToken
        , baseUrl = baseUrl
        , data = Loading
        , projectId = projectId
        }
        |> andPerform (Data.getData baseUrl apiToken projectId DataLoaded)


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        FormMsg subMsg ->
            case model.data of
                Loaded form project ->
                    case subMsg of
                        Form.SubmitResponse (Ok _) ->
                            return model
                                |> andPerform (Data.getData model.baseUrl model.apiToken model.projectId DataLoaded)

                        _ ->
                            Form.update subMsg form
                                |> Util.map (\f -> { model | data = Loaded f project }) FormMsg

                _ ->
                    return model

        DataLoaded (Ok response) ->
            case response.currentProject of
                Just project ->
                    let
                        updateModel form =
                            { model | data = Loaded form project }
                    in
                    Form.init model.baseUrl model.apiToken model.projectId response.allProjects
                        |> Util.map updateModel FormMsg

                Nothing ->
                    return model
                        |> andPerform (Route.redirectTo (Route.Protected (Route.NewProject Route.CreateProject)))

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
form form project =
    div []
        (List.map existingEnvironment project.environments
            ++ [ Form.view form
                    |> Html.map FormMsg
               ]
        )


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
            form f project

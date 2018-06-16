module App.Pages.ProjectsList exposing (..)

import App.Api.ListProjects as Api exposing (Project)
import App.Html exposing (..)
import Html exposing (Html)
import Http exposing (Error)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (..)
import Util exposing (PageHandler, andPerform, noop, return)


type alias Model =
    { projects : WebData (List Project), apiToken : String }


init : String -> PageHandler Model Msg
init apiToken =
    return { projects = Loading, apiToken = apiToken }
        |> andPerform (Api.listProjects apiToken ProjectsResponse)


type Msg
    = ProjectsResponse (Result Error (List Project))
    | CreateNewButtonClicked


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        ProjectsResponse result ->
            { model | projects = RemoteData.fromResult result }
                |> return

        CreateNewButtonClicked ->
            return model
                |> andPerform (redirectTo (Protected NewProject))


projectRow : Project -> Html msg
projectRow project =
    tr
        [ td
            [ text project.name
            ]
        , td
            [ text project.deploymentImage
            ]
        ]


content : Model -> Html Msg
content { projects } =
    case projects of
        NotAsked ->
            spinner

        Loading ->
            spinner

        Success projects ->
            table
                [ thead
                    [ tr
                        [ th [ text "Project name" ]
                        , th [ text "Deployment image" ]
                        ]
                    ]
                , tbody
                    (List.map projectRow projects)
                ]

        Failure e ->
            text (toString e)


view : Model -> Html Msg
view model =
    container
        [ pageHeaderWithActions "Projects List" [ Create "Create new" CreateNewButtonClicked ]
        , fullscreenCard [ content model ]
        ]

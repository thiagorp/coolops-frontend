module Route exposing
    ( AuthRoute(..)
    , NavigationKey
    , NewProjectStep(..)
    , ProtectedRoute(..)
    , PublicRoute(..)
    , Route(..)
    , authRoot
    , modifyTo
    , protectedRoot
    , redirectTo
    , routeFromLocation
    , toUrl
    )

import Browser.Navigation as Navigation
import Dict
import Url exposing (Url)
import Url.Parser as Url exposing (..)
import Url.Parser.Query as Query


type alias NavigationKey =
    Navigation.Key


type NewProjectStep
    = CreateProject
    | IntegrateWithSlack String Bool
    | IntegrateWithSlackCallback String String
    | CreateEnvironments String
    | IntegrateWithCI String


type ProtectedRoute
    = Home
    | ProjectsList
    | NewProject NewProjectStep
    | EditProject String
    | SyncingProject String String
    | NewEnvironment String
    | EditEnvironment String


type AuthRoute
    = Signup
    | Login


type PublicRoute
    = DeploymentLogs String
    | SlackCallback (Maybe String) (Maybe String)


type Route
    = Auth AuthRoute
    | Public PublicRoute
    | Protected ProtectedRoute


authRoot : Route
authRoot =
    Auth Login


protectedRoot : Route
protectedRoot =
    Protected Home


redirectTo : Navigation.Key -> Route -> Cmd msg
redirectTo key route =
    toUrl route
        |> Navigation.pushUrl key


modifyTo : Navigation.Key -> Route -> Cmd msg
modifyTo key route =
    toUrl route
        |> Navigation.replaceUrl key


routeFromLocation : Url -> Maybe Route
routeFromLocation =
    Url.parse
        (Url.oneOf
            [ Url.map Auth authRouteParser
            , Url.map Protected protectedRouteParser
            , Url.map Public publicRouteParser
            ]
        )


boolParam : String -> Query.Parser Bool
boolParam name =
    Query.enum name (Dict.fromList [ ( "true", True ), ( "false", False ) ])
        |> Query.map (Maybe.withDefault False)


authRouteParser : Url.Parser (AuthRoute -> a) a
authRouteParser =
    Url.oneOf
        [ Url.map Signup (s "signup")
        , Url.map Login (s "login")
        ]


publicRouteParser : Url.Parser (PublicRoute -> a) a
publicRouteParser =
    Url.oneOf
        [ Url.map DeploymentLogs (s "deployments" </> string </> s "logs")
        , Url.map SlackCallback (s "slack" </> s "callback" <?> Query.string "code" <?> Query.string "state")
        ]


newProjectRouteParser : Url.Parser (NewProjectStep -> a) a
newProjectRouteParser =
    Url.oneOf
        [ Url.map CreateProject (s "projects" </> s "new")
        , Url.map IntegrateWithSlack (s "projects" </> s "new" </> string </> s "slack-integration" <?> boolParam "error")
        , Url.map IntegrateWithSlackCallback (s "projects" </> s "new" </> string </> s "slack-integration" </> string)
        , Url.map CreateEnvironments (s "projects" </> s "new" </> string </> s "create-environments")
        , Url.map IntegrateWithCI (s "projects" </> s "new" </> string </> s "ci-integration")
        ]


protectedRouteParser : Url.Parser (ProtectedRoute -> a) a
protectedRouteParser =
    Url.oneOf
        [ Url.map Home top
        , Url.map NewProject newProjectRouteParser
        , Url.map EditProject (s "projects" </> string </> s "edit")
        , Url.map NewEnvironment (s "projects" </> string </> s "environments" </> s "new")
        , Url.map EditEnvironment (s "environments" </> string </> s "edit")
        , Url.map SyncingProject (s "projects" </> s "syncing" </> string </> string)
        ]


toUrl : Route -> String
toUrl route =
    case route of
        Auth Signup ->
            "/signup"

        Auth Login ->
            "/login"

        Public (DeploymentLogs deploymentId) ->
            "/deployments/" ++ deploymentId ++ "/logs"

        Public (SlackCallback _ _) ->
            "/slack/callback"

        Protected Home ->
            "/"

        Protected ProjectsList ->
            "/"

        Protected (SyncingProject code projectId) ->
            "/projects/syncing/" ++ code ++ "/" ++ projectId

        Protected (NewProject CreateProject) ->
            "/projects/new"

        Protected (NewProject (IntegrateWithSlack projectId error)) ->
            "/projects/new/"
                ++ projectId
                ++ "/slack-integration"
                ++ (if error then
                        "?error=true"

                    else
                        ""
                   )

        Protected (NewProject (IntegrateWithSlackCallback projectId code)) ->
            "/projects/new/" ++ projectId ++ "/slack-integration/" ++ code

        Protected (NewProject (CreateEnvironments projectId)) ->
            "/projects/new/" ++ projectId ++ "/create-environments"

        Protected (NewProject (IntegrateWithCI projectId)) ->
            "/projects/new/" ++ projectId ++ "/ci-integration"

        Protected (EditProject projectId) ->
            "/projects/" ++ projectId ++ "/edit"

        Protected (NewEnvironment projectId) ->
            "/projects/" ++ projectId ++ "/environments/new"

        Protected (EditEnvironment environmentId) ->
            "/environments/" ++ environmentId ++ "/edit"

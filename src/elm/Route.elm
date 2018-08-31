module Route exposing
    ( AuthRoute(..)
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

import Navigation
import UrlParser as Url exposing (..)


type ProtectedRoute
    = Home
    | ProjectsList
    | NewProject
    | EditProject String
    | SyncingProject (Maybe String) (Maybe String)
    | NewEnvironment String
    | CopyEnvironment String
    | EditEnvironment String


type AuthRoute
    = Signup
    | Login


type PublicRoute
    = DeploymentLogs String


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


redirectTo : Route -> Cmd msg
redirectTo route =
    toUrl route
        |> Navigation.newUrl


modifyTo : Route -> Cmd msg
modifyTo route =
    toUrl route
        |> Navigation.modifyUrl


routeFromLocation : Navigation.Location -> Maybe Route
routeFromLocation location =
    Url.parsePath
        (Url.oneOf
            [ Url.map Auth authRouteParser
            , Url.map Protected protectedRouteParser
            , Url.map Public publicRouteParser
            ]
        )
        location


authRouteParser : Url.Parser (AuthRoute -> a) a
authRouteParser =
    Url.oneOf
        [ Url.map Signup (s "__signup__")
        , Url.map Login (s "login")
        ]


publicRouteParser : Url.Parser (PublicRoute -> a) a
publicRouteParser =
    Url.oneOf
        [ Url.map DeploymentLogs (s "deployments" </> string </> s "logs")
        ]


protectedRouteParser : Url.Parser (ProtectedRoute -> a) a
protectedRouteParser =
    Url.oneOf
        [ Url.map Home top
        , Url.map NewProject (s "projects" </> s "new")
        , Url.map EditProject (s "projects" </> string </> s "edit")
        , Url.map NewEnvironment (s "projects" </> string </> s "environments" </> s "new")
        , Url.map EditEnvironment (s "environments" </> string </> s "edit")
        , Url.map CopyEnvironment (s "environments" </> string </> s "copy")
        , Url.map SyncingProject (s "projects" </> s "syncing" <?> stringParam "code" <?> stringParam "state")
        ]


toUrl : Route -> String
toUrl route =
    case route of
        Auth Signup ->
            "/__signup__"

        Auth Login ->
            "/login"

        Public (DeploymentLogs deploymentId) ->
            "/deployments/" ++ deploymentId ++ "/logs"

        Protected Home ->
            "/"

        Protected ProjectsList ->
            "/"

        Protected (SyncingProject code state) ->
            "/projects/syncing?code=" ++ Maybe.withDefault "" code ++ "&state=" ++ Maybe.withDefault "" state

        Protected NewProject ->
            "/projects/new"

        Protected (EditProject projectId) ->
            "/projects/" ++ projectId ++ "/edit"

        Protected (NewEnvironment projectId) ->
            "/projects/" ++ projectId ++ "/environments/new"

        Protected (EditEnvironment environmentId) ->
            "/environments/" ++ environmentId ++ "/edit"

        Protected (CopyEnvironment environmentId) ->
            "/environments/" ++ environmentId ++ "/copy"

module Route exposing
    ( AuthRoute(..)
    , ProtectedRoute(..)
    , Route(..)
    , authRoot
    , isAuthRoute
    , isProtectedRoute
    , modifyTo
    , protectedRoot
    , readAuthRoute
    , readProtectedRoute
    , redirectTo
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
    | DeploymentLogs String


type Route
    = Auth AuthRoute
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


isAuthRoute : Navigation.Location -> Bool
isAuthRoute location =
    case readAuthRoute location of
        Just _ ->
            True

        Nothing ->
            False


isProtectedRoute : Navigation.Location -> Bool
isProtectedRoute location =
    case readProtectedRoute location of
        Just _ ->
            True

        Nothing ->
            False


readAuthRoute : Navigation.Location -> Maybe AuthRoute
readAuthRoute =
    Url.parsePath authRouteParser


readProtectedRoute : Navigation.Location -> Maybe ProtectedRoute
readProtectedRoute =
    Url.parsePath procetedRouteParser


authRouteParser : Url.Parser (AuthRoute -> a) a
authRouteParser =
    Url.oneOf
        [ Url.map Signup (s "__signup__")
        , Url.map Login (s "login")
        , Url.map DeploymentLogs (s "deployments" </> string </> s "logs")
        ]


procetedRouteParser : Url.Parser (ProtectedRoute -> a) a
procetedRouteParser =
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

        Auth Signup ->
            "/__signup__"

        Auth Login ->
            "/login"

        Auth (DeploymentLogs deploymentId) ->
            "/deployments/" ++ deploymentId ++ "/logs"

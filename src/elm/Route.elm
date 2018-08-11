module Route exposing (..)

import Navigation
import UrlParser as Url exposing (..)


type ProtectedRoute
    = ProjectsList
    | NewProject
    | EditProject String
    | SyncingProject (Maybe String) (Maybe String)
    | NewEnvironment String
    | CopyEnvironment String
    | EditEnvironment String
    | Settings (Maybe String)


type OpenRoute
    = Signup
    | Login


type Route
    = Open OpenRoute
    | Protected ProtectedRoute


openRoot : Route
openRoot =
    Open Login


protectedRoot : Route
protectedRoot =
    Protected ProjectsList


redirectTo : Route -> Cmd msg
redirectTo route =
    toUrl route
        |> Navigation.newUrl


modifyTo : Route -> Cmd msg
modifyTo route =
    toUrl route
        |> Navigation.modifyUrl


isOpenRoute : Navigation.Location -> Bool
isOpenRoute location =
    case readOpenRoute location of
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


readOpenRoute : Navigation.Location -> Maybe OpenRoute
readOpenRoute =
    Url.parsePath openRouteParser


readProtectedRoute : Navigation.Location -> Maybe ProtectedRoute
readProtectedRoute =
    Url.parsePath procetedRouteParser


openRouteParser : Url.Parser (OpenRoute -> a) a
openRouteParser =
    Url.oneOf
        [ Url.map Signup (s "signup")
        , Url.map Login (s "login")
        ]


procetedRouteParser : Url.Parser (ProtectedRoute -> a) a
procetedRouteParser =
    Url.oneOf
        [ Url.map ProjectsList top
        , Url.map NewProject (s "projects" </> s "new")
        , Url.map EditProject (s "projects" </> string </> s "edit")
        , Url.map NewEnvironment (s "projects" </> string </> s "environments" </> s "new")
        , Url.map EditEnvironment (s "environments" </> string </> s "edit")
        , Url.map CopyEnvironment (s "environments" </> string </> s "copy")
        , Url.map SyncingProject (s "projects" </> s "syncing" <?> stringParam "code" <?> stringParam "state")
        , Url.map Settings (s "settings" <?> stringParam "code")
        ]


toUrl : Route -> String
toUrl route =
    case route of
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

        Protected (Settings _) ->
            "/settings"

        Open Signup ->
            "/signup"

        Open Login ->
            "/login"

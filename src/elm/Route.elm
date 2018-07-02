module Route exposing (..)

import Navigation
import UrlParser as Url exposing ((</>), (<?>), int, s, stringParam, top)


type ProtectedRoute
    = ProjectsList
    | NewProject
    | EditProject String
    | NewEnvironment String
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
        , Url.map Settings (s "settings" <?> stringParam "code")
        ]


toUrl : Route -> String
toUrl route =
    case route of
        Protected ProjectsList ->
            "/"

        Protected NewProject ->
            "/projects/new"

        Protected (EditProject projectId) ->
            "/projects/" ++ projectId ++ "/edit"

        Protected (NewEnvironment projectId) ->
            "/projects/" ++ projectId ++ "/environments/new"

        Protected (Settings _) ->
            "/settings"

        Open Signup ->
            "/signup"

        Open Login ->
            "/login"

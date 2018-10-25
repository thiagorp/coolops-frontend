module App.Pages.SlackCallback exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Api
import App.Api.CreateSlackAccessToken exposing (..)
import Html exposing (..)
import Http
import NotFound
import Route
import Util exposing (PageHandler, andPerform, return)


type Msg
    = SyncResponse (Result Http.Error ())


type NextPage
    = ProjectCreation
    | ProjectSettings


type State
    = Sending String NextPage
    | InvalidParams


type alias Model =
    { state : State
    , navigationKey : Route.NavigationKey
    }


init : Api.ProtectedConfig -> Route.NavigationKey -> Maybe String -> Maybe String -> PageHandler Model Msg
init apiConfig navigationKey maybeCode maybeState =
    case Maybe.map2 (\a b -> ( a, b )) maybeCode maybeState of
        Nothing ->
            return { navigationKey = navigationKey, state = InvalidParams }

        Just ( code, state ) ->
            case String.split "|" state of
                "newProject" :: projectId :: [] ->
                    return
                        { navigationKey = navigationKey
                        , state = Sending projectId ProjectCreation
                        }
                        |> andPerform (createSlackAccessToken apiConfig SyncResponse { code = code })

                projectId :: [] ->
                    return
                        { navigationKey = navigationKey
                        , state = Sending projectId ProjectSettings
                        }
                        |> andPerform (createSlackAccessToken apiConfig SyncResponse { code = code })

                _ ->
                    return { navigationKey = navigationKey, state = InvalidParams }


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        SyncResponse result ->
            case model.state of
                Sending projectId ProjectCreation ->
                    let
                        hasError =
                            case result of
                                Ok _ ->
                                    False

                                Err _ ->
                                    True

                        route =
                            Route.Protected (Route.NewProject (Route.IntegrateWithSlack projectId hasError))
                    in
                    return model
                        |> andPerform (Route.modifyTo model.navigationKey route)

                Sending projectId ProjectSettings ->
                    let
                        route =
                            Route.Protected (Route.EditProject projectId)
                    in
                    return model
                        |> andPerform (Route.modifyTo model.navigationKey route)

                _ ->
                    return model


view : Model -> Html Msg
view model =
    case model.state of
        InvalidParams ->
            NotFound.view

        Sending _ _ ->
            div [] []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

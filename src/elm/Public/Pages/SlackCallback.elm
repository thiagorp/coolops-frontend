module Public.Pages.SlackCallback exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Html exposing (..)
import NotFound
import Route
import Util exposing (PageHandler, andPerform, return)


type Msg
    = Noop


type alias Model =
    { notFound : Bool }


init : Route.NavigationKey -> Maybe String -> Maybe String -> PageHandler Model Msg
init navigationKey maybeCode maybeState =
    case Maybe.map2 (\a b -> ( a, b )) maybeCode maybeState of
        Nothing ->
            return { notFound = True }

        Just ( code, state ) ->
            case String.split "|" state of
                "newProject" :: projectId :: [] ->
                    return { notFound = False }
                        |> andPerform (Route.modifyTo navigationKey (Route.Protected (Route.NewProject (Route.IntegrateWithSlackCallback projectId code))))

                projectId :: [] ->
                    return { notFound = False }
                        |> andPerform (Route.modifyTo navigationKey (Route.Protected (Route.SyncingProject code projectId)))

                _ ->
                    return { notFound = True }


update : Msg -> Model -> PageHandler Model Msg
update _ model =
    return model


view : Model -> Html Msg
view model =
    if model.notFound then
        NotFound.view

    else
        div [] []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

module App.Pages.Projects.New.CreateProject.Main exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import App.Api.CreateProject as Api
import App.Forms.Projects.Main as Form
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Route
import Util exposing (PageHandler, andPerform, andPerform_, noop, return)


type alias Model =
    { form : Form.Model
    , navigationKey : Route.NavigationKey
    }


type Msg
    = FormMsg Form.Msg


init : String -> String -> Route.NavigationKey -> PageHandler Model Msg
init baseUrl apiToken navigationKey =
    let
        ( formModel, formCmd ) =
            Form.init baseUrl apiToken Form.Create
                |> Util.map identity FormMsg
    in
    return
        { form = formModel
        , navigationKey = navigationKey
        }
        |> andPerform_ formCmd


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        FormMsg subMsg ->
            case subMsg of
                Form.SubmitResponse (Ok response) ->
                    let
                        redirect =
                            Route.IntegrateWithSlack response.id False
                                |> Route.NewProject
                                |> Route.Protected
                                |> Route.redirectTo model.navigationKey
                    in
                    return model
                        |> andPerform redirect

                _ ->
                    Form.update subMsg model.form
                        |> Util.map (\f -> { model | form = f }) FormMsg


view : Model -> Html Msg
view { form } =
    Html.form [ class "card", onSubmit Form.Submit ]
        [ div [ class "card-body" ] (Form.view form)
        , div [ class "card-footer text-right" ]
            [ button [ type_ "submit", class "btn btn-primary" ] [ text "Continue" ] ]
        ]
        |> Html.map FormMsg

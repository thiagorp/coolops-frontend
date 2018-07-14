module App.Fragments.Topbar exposing (Model, Msg, init, subscriptions, update, view)

import App.Api.GetProfile as Api exposing (Profile)
import App.Html as AppHtml
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error)
import Mouse
import Ports
import RemoteData exposing (RemoteData(..), WebData)
import Route
import Util exposing (PageHandler, andPerform, noop, return)


type Msg
    = Toggle
    | CloseDropdown
    | RedirectTo Route.Route
    | ProfileLoaded (Result Error Profile)
    | LogOut


type alias Model =
    { dropdownOpened : Bool
    , profile : WebData Profile
    }


init : String -> String -> PageHandler Model Msg
init baseUrl apiToken =
    return { dropdownOpened = False, profile = Loading }
        |> andPerform (Api.getProfile baseUrl apiToken ProfileLoaded)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dropdownOpened of
        True ->
            Mouse.clicks (\_ -> CloseDropdown)

        False ->
            Sub.none


toggleDropdown : Model -> Model
toggleDropdown model =
    { model | dropdownOpened = not model.dropdownOpened }


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        ProfileLoaded result ->
            { model | profile = RemoteData.fromResult result }
                |> return

        Toggle ->
            model
                |> toggleDropdown
                |> return

        CloseDropdown ->
            return { model | dropdownOpened = False }

        RedirectTo route ->
            model
                |> toggleDropdown
                |> return
                |> andPerform (Route.redirectTo route)

        LogOut ->
            return model
                |> andPerform (Ports.logout ())


dropdownMenu : List (Html Msg)
dropdownMenu =
    [ AppHtml.a (Route.Protected (Route.Settings Nothing))
        RedirectTo
        [ class "dropdown-item" ]
        [ i [ class "dropdown-icon fe fe-settings" ] []
        , text "Settings"
        ]
    , div [ class "dropdown-divider" ] []
    , a [ class "dropdown-item", href "#", onClick LogOut ]
        [ i [ class "dropdown-icon fe fe-log-out" ] []
        , text "Sign out"
        ]
    ]


dropdown : Model -> Html Msg
dropdown model =
    case model.profile of
        Success profile ->
            div [ class "dropdown" ]
                [ a
                    [ class "nav-link pr-0 leading-none"
                    , attribute "data-toggle" "dropdown"
                    , onClick Toggle
                    ]
                    [ span [ class "avatar" ] [ text (String.left 1 profile.user.first_name ++ String.left 1 profile.user.last_name) ]
                    , span
                        [ class "ml-2 d-none d-lg-block" ]
                        [ span [ class "text-default" ] [ text (profile.user.first_name ++ " " ++ profile.user.last_name) ]
                        , small [ class "text-muted d-block mt-1" ] [ text profile.company.name ]
                        ]
                    ]
                , div
                    [ classList
                        [ ( "dropdown-menu dropdown-menu-right dropdown-menu-arrow", True )
                        , ( "show", model.dropdownOpened )
                        ]
                    ]
                    dropdownMenu
                ]

        _ ->
            div [] []


logo : Html msg
logo =
    a [ class "header-brand" ] [ text "CoolOps.io" ]


view : Model -> Html Msg
view model =
    div [ class "header py-4" ]
        [ div [ class "container" ]
            [ div [ class "d-flex" ]
                [ logo
                , div [ class "d-flex order-lg-2 ml-auto" ]
                    [ dropdown model ]
                ]
            ]
        ]

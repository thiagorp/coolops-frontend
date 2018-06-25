module App.Fragments.Topbar exposing (Model, Msg, init, update, view)

import App.Html as AppHtml
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ports
import Route
import Util exposing (PageHandler, andPerform, noop, return)


type Msg
    = Toggle
    | RedirectTo Route.Route
    | LogOut


type alias Model =
    { dropdownOpened : Bool }


init : PageHandler Model Msg
init =
    { dropdownOpened = False }
        |> return


toggleDropdown : Model -> Model
toggleDropdown model =
    { model | dropdownOpened = not model.dropdownOpened }


update : Msg -> Model -> PageHandler Model Msg
update msg model =
    case msg of
        Toggle ->
            model
                |> toggleDropdown
                |> return

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
    div [ class "dropdown" ]
        [ a
            [ class "nav-link pr-0 leading-none"
            , attribute "data-toggle" "dropdown"
            , onClick Toggle
            ]
            [ span [ class "avatar" ] [ text "CO" ]
            , span
                [ class "ml-2 d-none d-lg-block" ]
                [ span [ class "text-default" ] [ text "{{USER_NAME}}" ]
                , small [ class "text-muted d-block mt-1" ] [ text "{{COMPANY_NAME}}" ]
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

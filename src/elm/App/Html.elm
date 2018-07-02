module App.Html exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Route exposing (..)


container : List (Html msg) -> Html msg
container children =
    div [ class "container" ] children


pageHeader : String -> Html msg
pageHeader title =
    div [ class "page-header" ]
        [ h1 [ class "page-title" ] [ text title ] ]


type Action msg
    = Create String msg


actionButton : Action msg -> Html msg
actionButton action =
    case action of
        Create buttonTitle clickHandler ->
            Html.button [ class "btn btn-success", onClick clickHandler ]
                [ text buttonTitle ]


type ButtonSize
    = NormalSize
    | Sm
    | Lg


type ButtonColor
    = Primary
    | Secondary
    | Danger


type Icon
    = Slack


i : Icon -> List (Attribute msg) -> Html msg
i iconType attrs =
    let
        iconClass =
            case iconType of
                Slack ->
                    "fab fa-slack-hash"
    in
    Html.i ([ class iconClass ] ++ attrs) []


type StatusIcon
    = StatusIconSuccess


statusIcon : StatusIcon -> Html msg
statusIcon state =
    let
        bgClass =
            case state of
                StatusIconSuccess ->
                    "bg-success"
    in
    Html.span [ class "status-icon", class bgClass ] []


type alias ButtonConfig =
    { size : ButtonSize
    , color : ButtonColor
    , icon : Maybe Icon
    , text : String
    }


buttonConfig : ButtonConfig
buttonConfig =
    { size = NormalSize
    , color = Primary
    , icon = Nothing
    , text = ""
    }


button : msg -> ButtonConfig -> Html msg
button msg config =
    let
        sizeAttr =
            case config.size of
                NormalSize ->
                    []

                Sm ->
                    [ class "btn-sm" ]

                Lg ->
                    [ class "btn-lg" ]

        colorAttr =
            case config.color of
                Primary ->
                    [ class "btn-primary" ]

                Secondary ->
                    [ class "btn-secondary" ]

                Danger ->
                    [ class "btn-danger" ]

        attributes =
            [ class "btn", onClick msg, type_ "button" ] ++ sizeAttr ++ colorAttr

        icon =
            case config.icon of
                Nothing ->
                    []

                Just icon ->
                    [ i icon [ class "mr-2" ] ]
    in
    Html.button attributes (icon ++ [ text config.text ])


pageHeaderWithActions : String -> List (Action msg) -> Html msg
pageHeaderWithActions title actions =
    div [ class "page-header" ]
        [ h1 [ class "page-title" ] [ text title ]
        , div [ class "page-options d-flex" ]
            (List.map actionButton actions)
        ]


row : List (Html msg) -> Html msg
row =
    div [ class "row" ]


type ColSize
    = Full
    | Half
    | OneFourth
    | OneThird


type alias ColConfig =
    { colSmSize : ColSize, colMdSize : ColSize, colLgSize : ColSize }


colSize : ColSize -> String
colSize size =
    case size of
        Full ->
            "12"

        Half ->
            "6"

        OneThird ->
            "4"

        OneFourth ->
            "3"


col : ColConfig -> List (Html msg) -> Html msg
col { colSmSize, colMdSize, colLgSize } =
    div
        [ class "col"
        , class ("col-sm-" ++ colSize colSmSize)
        , class ("col-md-" ++ colSize colMdSize)
        , class ("col-lg-" ++ colSize colLgSize)
        ]


spinner : Html msg
spinner =
    div [ style [ ( "padding", "60px" ), ( "text-align", "center" ), ( "width", "100%" ) ] ]
        [ div [ class "spinner" ]
            [ div [ class "bounce1" ] []
            , div [ class "bounce2" ] []
            , div [ class "bounce3" ] []
            ]
        ]


fullscreenCard : List (Html msg) -> Html msg
fullscreenCard children =
    div [ class "row row-cards row-deck" ]
        [ div [ class "col-12" ]
            [ div [ class "card" ] children ]
        ]


cardWithTitle : String -> List (Html msg) -> Html msg
cardWithTitle title children =
    div [ class "card" ]
        ([ div [ class "card-header" ] [ h3 [ class "card-title" ] [ text title ] ] ] ++ children)


cardBody : List (Html msg) -> Html msg
cardBody children =
    div [ class "card-body" ] children


table : List (Html msg) -> Html msg
table children =
    Html.table [ class "table table-hover table-outline table-vcenter card-table" ] children


thead : List (Html msg) -> Html msg
thead =
    Html.thead []


tbody : List (Html msg) -> Html msg
tbody =
    Html.tbody []


tr : List (Html msg) -> Html msg
tr =
    Html.tr []


th : List (Html msg) -> Html msg
th =
    Html.th []


td : List (Html msg) -> Html msg
td =
    Html.td []


text : String -> Html msg
text =
    Html.text


img : String -> List (Attribute msg) -> Html msg
img url customAttributes =
    let
        attributes =
            customAttributes ++ [ src url ]
    in
    Html.img attributes []


externalLink : String -> List (Attribute msg) -> List (Html msg) -> Html msg
externalLink url customAttributes children =
    let
        attributes =
            customAttributes ++ [ href url ]
    in
    Html.a attributes children


a : Route -> (Route -> msg) -> List (Attribute msg) -> List (Html msg) -> Html msg
a route msg customAttributes children =
    let
        attributes =
            customAttributes
                ++ [ href (toUrl route), onPreventDefaultClick (msg route) ]
    in
    Html.a attributes children



-- See https://github.com/elm-lang/html/issues/110


onPreventDefaultClick : msg -> Attribute msg
onPreventDefaultClick message =
    onWithOptions "click"
        { defaultOptions | preventDefault = True }
        (preventDefault2
            |> Json.Decode.andThen (maybePreventDefault message)
        )


preventDefault2 : Json.Decode.Decoder Bool
preventDefault2 =
    Json.Decode.map2
        invertedOr
        (Json.Decode.field "ctrlKey" Json.Decode.bool)
        (Json.Decode.field "metaKey" Json.Decode.bool)


maybePreventDefault : msg -> Bool -> Json.Decode.Decoder msg
maybePreventDefault msg preventDefault =
    case preventDefault of
        True ->
            Json.Decode.succeed msg

        False ->
            Json.Decode.fail "Normal link"


invertedOr : Bool -> Bool -> Bool
invertedOr x y =
    not (x || y)



-- End https://github.com/elm-lang/html/issues/110

module Form.Html exposing
    ( Input
    , InputAttribute(..)
    , InputType(..)
    , input
    , submitButton
    )

import Html
import Html.Attributes as Attr
import Html.Events as Events


type InputType
    = Text
    | Email
    | Password


type InputAttribute msg
    = InputType InputType
    | InputValue String
    | OnInput (String -> msg)


type alias Input msg =
    { label : String
    , placeholder : String
    , errors : Maybe (List String)
    , disabled : Bool
    , attributes : List (InputAttribute msg)
    }


inputType : InputType -> String
inputType t =
    case t of
        Text ->
            "text"

        Email ->
            "email"

        Password ->
            "password"


toHtmlAttribute : InputAttribute msg -> Html.Attribute msg
toHtmlAttribute attr =
    case attr of
        InputType t ->
            Attr.type_ (inputType t)

        InputValue v ->
            Attr.value v

        OnInput msg ->
            Events.onInput msg


addAttributes : List (InputAttribute msg) -> List (Html.Attribute msg) -> List (Html.Attribute msg)
addAttributes attributes htmlAttributes =
    htmlAttributes ++ List.map toHtmlAttribute attributes


addErrorClass : Maybe (List String) -> List (Html.Attribute msg) -> List (Html.Attribute msg)
addErrorClass maybeErrors htmlAttributes =
    case maybeErrors of
        Nothing ->
            htmlAttributes

        Just [] ->
            htmlAttributes ++ [ Attr.class "is-valid" ]

        Just _ ->
            htmlAttributes ++ [ Attr.class "is-invalid" ]


addErrorMessage : Maybe (List String) -> List (Html.Html msg) -> List (Html.Html msg)
addErrorMessage maybeErrors elements =
    let
        errorMessage errors =
            [ Html.div
                [ Attr.class "invalid-feedback" ]
                [ Html.text (Maybe.withDefault "" (List.head errors)) ]
            ]
    in
    case maybeErrors of
        Nothing ->
            elements

        Just [] ->
            elements

        Just errors ->
            elements ++ errorMessage errors


input : Input msg -> Html.Html msg
input { label, placeholder, errors, disabled, attributes } =
    let
        inputAttributes =
            [ Attr.class "form-control", Attr.placeholder placeholder, Attr.disabled disabled ]
                |> addAttributes attributes
                |> addErrorClass errors

        elements =
            [ Html.label [ Attr.class "form-label" ] [ Html.text label ]
            , Html.input inputAttributes []
            ]
                |> addErrorMessage errors
    in
    Html.div [ Attr.class "form-group" ] elements


submitButton : String -> Bool -> Html.Html msg
submitButton text loading =
    let
        addLoading attrs =
            case loading of
                True ->
                    attrs ++ [ Attr.class "btn-loading", Attr.disabled True ]

                False ->
                    attrs

        attributes =
            [ Attr.class "btn btn-primary btn-block", Attr.type_ "submit" ]
                |> addLoading
    in
    Html.button attributes [ Html.text text ]

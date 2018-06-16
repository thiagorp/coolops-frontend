module App.Html.Form
    exposing
        ( Input(..)
        , InputAttribute(..)
        , linearCardForm
        )

import Html as Html
import Html.Attributes as Attr
import Html.Events as Events


type InputAttribute msg
    = InputValue String
    | OnInput (String -> msg)


type alias TextInputConfig msg =
    { label : String
    , placeholder : String
    , errors : Maybe (List String)
    , disabled : Bool
    , attributes : List (InputAttribute msg)
    , id : String
    }


type Input msg
    = TextInput (TextInputConfig msg)
    | EmailInput (TextInputConfig msg)
    | PasswordInput (TextInputConfig msg)


inputAttributeToHtml : InputAttribute msg -> Html.Attribute msg
inputAttributeToHtml attr =
    case attr of
        InputValue v ->
            Attr.value v

        OnInput msg ->
            Events.onInput msg


addAttributesToInput : List (InputAttribute msg) -> List (Html.Attribute msg) -> List (Html.Attribute msg)
addAttributesToInput attributes htmlAttributes =
    htmlAttributes ++ List.map inputAttributeToHtml attributes


addErrorMessageToInput : Maybe (List String) -> List (Html.Html msg) -> List (Html.Html msg)
addErrorMessageToInput maybeErrors elements =
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


addErrorClassToInput : Maybe (List String) -> List (Html.Attribute msg) -> List (Html.Attribute msg)
addErrorClassToInput maybeErrors htmlAttributes =
    case maybeErrors of
        Nothing ->
            htmlAttributes

        Just [] ->
            htmlAttributes ++ [ Attr.class "is-valid" ]

        Just _ ->
            htmlAttributes ++ [ Attr.class "is-invalid" ]


textInputHtml : String -> TextInputConfig msg -> Html.Html msg
textInputHtml inputType { label, placeholder, errors, disabled, attributes, id } =
    let
        inputAttributes =
            [ Attr.class "form-control"
            , Attr.placeholder placeholder
            , Attr.disabled disabled
            , Attr.id id
            , Attr.type_ inputType
            ]
                |> addAttributesToInput attributes
                |> addErrorClassToInput errors

        elements =
            [ Html.label [ Attr.class "form-label", Attr.for id ] [ Html.text label ]
            , Html.input inputAttributes []
            ]
                |> addErrorMessageToInput errors
    in
    Html.div [ Attr.class "form-group" ] elements


type alias FormConfig msg =
    { loading : Bool
    , error : Maybe String
    , submitButtonText : String
    , msg : msg
    }


input : Input msg -> Html.Html msg
input obj =
    case obj of
        TextInput config ->
            textInputHtml "text" config

        PasswordInput config ->
            textInputHtml "password" config

        EmailInput config ->
            textInputHtml "email" config


submitButton : String -> Bool -> Html.Html msg
submitButton text loading =
    let
        addLoading loading attributes =
            case loading of
                True ->
                    attributes ++ [ Attr.class "btn-loading", Attr.disabled True ]

                False ->
                    attributes

        attributes =
            [ Attr.class "btn btn-primary", Attr.type_ "submit" ]
                |> addLoading loading
    in
    Html.button attributes [ Html.text text ]


linearCardForm : FormConfig msg -> List (Input msg) -> Html.Html msg
linearCardForm { error, loading, submitButtonText, msg } inputs =
    let
        inputsHtml =
            List.map input inputs

        errorHtml =
            case error of
                Nothing ->
                    []

                Just error ->
                    [ Html.p [ Attr.class "text-red" ] [ Html.text error ] ]

        button =
            [ submitButton submitButtonText loading ]

        formBody =
            inputsHtml ++ errorHtml
    in
    Html.form [ Attr.class "card", Events.onSubmit msg ]
        [ Html.div [ Attr.class "card-body" ] formBody
        , Html.div [ Attr.class "card-footer text-right" ] button
        ]

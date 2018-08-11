module App.Html.Form
    exposing
        ( Input(..)
        , InputAttribute(..)
        , input
        , linearCardForm
        )

import Dict exposing (Dict)
import Html as Html
import Html.Attributes as Attr
import Html.Events as Events
import SelectList exposing (..)


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


type alias DropdownInputConfig msg =
    { label : String
    , options : SelectList { key : String, val : String }
    , errors : Maybe (List String)
    , disabled : Bool
    , events :
        { onSelect : String -> msg }
    , id : String
    }


type alias KeyValueInputConfig msg =
    { label : String
    , placeholder : { key : String, val : String }
    , errors : { key : Maybe (List String), val : Maybe (List String) }
    , disabled : Bool
    , id : String
    , events :
        { onKeyChange : String -> msg
        , onValueChange : String -> msg
        , onEntryRemove : String -> msg
        , onEntryEdit : String -> msg
        , onEntryAdd : msg
        }
    , values :
        { added : Dict String String
        , editing : { key : String, val : String }
        }
    }


type alias StaticTextInputConfig =
    { label : String, val : String }


type Input msg
    = TextInput (TextInputConfig msg)
    | EmailInput (TextInputConfig msg)
    | PasswordInput (TextInputConfig msg)
    | DropdownInput (DropdownInputConfig msg)
    | StaticTextInput StaticTextInputConfig
    | KeyValueInput (KeyValueInputConfig msg)


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


dropdownInputHtml : DropdownInputConfig msg -> Html.Html msg
dropdownInputHtml config =
    let
        options =
            config.options
                |> SelectList.mapBy
                    (\position option ->
                        Html.option
                            [ Attr.selected (position == SelectList.Selected), Attr.value option.key ]
                            [ Html.text option.val ]
                    )
                |> SelectList.toList

        dropdownAttributes =
            [ Attr.class "form-control"
            , Attr.disabled config.disabled
            , Attr.id config.id
            , Events.onInput config.events.onSelect
            ]
                |> addErrorClassToInput config.errors

        dropdown =
            Html.select dropdownAttributes options

        elements =
            [ Html.label [ Attr.class "form-label", Attr.for config.id ] [ Html.text config.label ]
            , dropdown
            ]
                |> addErrorMessageToInput config.errors
    in
    Html.div [ Attr.class "form-group" ] elements


keyValueInputHtml : KeyValueInputConfig msg -> Html.Html msg
keyValueInputHtml config =
    let
        addedRow key val inputs =
            inputs
                ++ [ Html.div [ Attr.class "col col-12 col-lg-5" ]
                        [ Html.div [ Attr.class "form-group" ]
                            [ Html.input
                                [ Attr.class "form-control"
                                , Attr.value key
                                , Attr.disabled True
                                ]
                                []
                            ]
                        ]
                   , Html.div [ Attr.class "col col-12 col-lg-5" ]
                        [ Html.div [ Attr.class "form-group" ]
                            [ Html.input
                                [ Attr.class "form-control"
                                , Attr.value val
                                , Attr.disabled True
                                ]
                                []
                            ]
                        ]
                   , Html.div [ Attr.class "col col-12 col-lg-2" ]
                        [ Html.div [ Attr.class "form-group" ]
                            [ Html.button
                                [ Attr.class "btn btn-icon btn-secondary mr-2"
                                , Events.onClick (config.events.onEntryEdit key)
                                , Attr.type_ "button"
                                , Attr.disabled config.disabled
                                ]
                                [ Html.i [ Attr.class "fas fa-pencil-alt" ] [] ]
                            , Html.button
                                [ Attr.class "btn btn-icon btn-secondary"
                                , Events.onClick (config.events.onEntryRemove key)
                                , Attr.type_ "button"
                                , Attr.disabled config.disabled
                                ]
                                [ Html.i [ Attr.class "far fa-trash-alt" ] [] ]
                            ]
                        ]
                   ]

        editingRow =
            [ Html.div [ Attr.class "col col-12 col-lg-5" ]
                [ Html.div [ Attr.class "form-group" ]
                    ([ Html.input
                        ([ Attr.class "form-control"
                         , Attr.value config.values.editing.key
                         , Attr.placeholder config.placeholder.key
                         , Events.onInput config.events.onKeyChange
                         , Attr.disabled config.disabled
                         , Attr.id (config.id ++ "-key")
                         ]
                            |> addErrorClassToInput config.errors.key
                        )
                        []
                     ]
                        |> addErrorMessageToInput config.errors.key
                    )
                ]
            , Html.div [ Attr.class "col col-12 col-lg-5" ]
                [ Html.div [ Attr.class "form-group" ]
                    ([ Html.input
                        ([ Attr.class "form-control"
                         , Attr.value config.values.editing.val
                         , Attr.placeholder config.placeholder.val
                         , Events.onInput config.events.onValueChange
                         , Attr.disabled config.disabled
                         , Attr.id (config.id ++ "-val")
                         ]
                            |> addErrorClassToInput config.errors.val
                        )
                        []
                     ]
                        |> addErrorMessageToInput config.errors.val
                    )
                ]
            , Html.div [ Attr.class "col col-12 col-lg-2" ]
                [ Html.button
                    [ Attr.class "btn btn-icon btn-primary btn-block"
                    , Events.onClick config.events.onEntryAdd
                    , Attr.type_ "button"
                    , Attr.disabled config.disabled
                    ]
                    [ Html.text "Add" ]
                ]
            ]

        inputs =
            Dict.foldr addedRow [] config.values.added
                |> (\xs -> xs ++ editingRow)
                |> Html.div [ Attr.class "row" ]
    in
    Html.div []
        [ Html.label [ Attr.class "form-label" ] [ Html.text config.label ]
        , inputs
        ]


staticTextInputHtml : StaticTextInputConfig -> Html.Html msg
staticTextInputHtml { label, val } =
    Html.div [ Attr.class "form-group" ]
        [ Html.label [ Attr.class "form-label" ] [ Html.text label ]
        , Html.div [ Attr.class "form-control-plaintext" ] [ Html.text val ]
        ]


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

        DropdownInput config ->
            dropdownInputHtml config

        StaticTextInput config ->
            staticTextInputHtml config

        KeyValueInput config ->
            keyValueInputHtml config


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


linearCardFormWithTitle : FormConfig msg -> Maybe String -> List (Input msg) -> Html.Html msg
linearCardFormWithTitle { error, loading, submitButtonText, msg } maybeTitle inputs =
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

        titleHtml =
            case maybeTitle of
                Nothing ->
                    []

                Just title ->
                    [ Html.div
                        [ Attr.class "card-header" ]
                        [ Html.h3 [ Attr.class "card-title" ] [ Html.text title ] ]
                    ]

        formBody =
            inputsHtml ++ errorHtml
    in
    Html.form [ Attr.class "card", Events.onSubmit msg ]
        [ Html.div [ Attr.class "card-body" ] formBody
        , Html.div [ Attr.class "card-footer text-right" ] button
        ]


linearCardForm : FormConfig msg -> List (Input msg) -> Html.Html msg
linearCardForm config inputs =
    linearCardFormWithTitle config Nothing inputs

module Form.Validation exposing
    ( FormConfig
    , FormState
    , Validator
    , addError
    , all
    , errorsOf
    , getServerError
    , ifBlank
    , ifInvalidEmail
    , ifInvalidSlug
    , ifShorterThan
    , initialState
    , isSubmitting
    , serverError
    , submit
    , uncategorizedError
    , validate
    )

import Util.Slug as Slug
import Validate as ElmValidate


type FormState fieldName
    = NotSubmitted
    | FixingErrors (List ( fieldName, String ))
    | ServerError String
    | Submitting


type alias ModelWithFormState model fieldName =
    { model | formState : FormState fieldName }


type alias Validator fieldName model =
    ElmValidate.Validator ( fieldName, String ) model


type alias ValidatorForFormState model fieldName =
    Validator fieldName (ModelWithFormState model fieldName)


type alias FormConfig model fieldName result =
    { validator : ValidatorForFormState model fieldName
    , successCallback : ModelWithFormState model fieldName -> result
    , errorCallback : ModelWithFormState model fieldName -> result
    }


initialState : FormState fieldName
initialState =
    NotSubmitted


isSubmitting : FormState fieldName -> Bool
isSubmitting formState =
    formState == Submitting


uncategorizedError : String -> ModelWithFormState model fieldName -> ModelWithFormState model fieldName
uncategorizedError error model =
    { model | formState = ServerError error }


getServerError : FormState fieldName -> Maybe String
getServerError formState =
    case formState of
        ServerError error ->
            Just error

        _ ->
            Nothing


serverError : ModelWithFormState model fieldName -> ModelWithFormState model fieldName
serverError model =
    { model | formState = ServerError "Something unexpected happened but we were already notified! Please try again later." }


addError :
    fieldName
    -> String
    -> ModelWithFormState model fieldName
    -> ModelWithFormState model fieldName
addError field string model =
    case model.formState of
        FixingErrors errors ->
            { model | formState = FixingErrors (( field, string ) :: errors) }

        _ ->
            { model | formState = FixingErrors [ ( field, string ) ] }


validate :
    FormConfig model fieldName result
    -> ModelWithFormState model fieldName
    -> ModelWithFormState model fieldName
validate { validator } model =
    case model.formState of
        FixingErrors _ ->
            case ElmValidate.validate validator model of
                Ok _ ->
                    { model | formState = FixingErrors [] }

                Err errors ->
                    { model | formState = FixingErrors errors }

        _ ->
            model


submit :
    FormConfig model fieldName result
    -> ModelWithFormState model fieldName
    -> result
submit { validator, successCallback, errorCallback } model =
    case ElmValidate.validate validator model of
        Ok _ ->
            successCallback { model | formState = Submitting }

        Err errors ->
            errorCallback { model | formState = FixingErrors errors }


errorsOf : FormState fieldName -> fieldName -> Maybe (List String)
errorsOf formState field =
    case formState of
        Submitting ->
            Nothing

        FixingErrors errors ->
            errors
                |> List.filter (\( name, _ ) -> name == field)
                |> List.map (\( _, error ) -> error)
                |> Just

        _ ->
            Nothing


all : List (Validator fieldName model) -> Validator fieldName model
all =
    ElmValidate.all


ifBlank : (model -> String) -> fieldName -> Validator fieldName model
ifBlank getter fieldName =
    ElmValidate.ifBlank getter ( fieldName, "Cannot be blank" )


ifInvalidEmail : (model -> String) -> fieldName -> Validator fieldName model
ifInvalidEmail getter fieldName =
    ElmValidate.ifInvalidEmail getter (\_ -> ( fieldName, "Is not a valid email" ))


ifInvalidSlug : (model -> String) -> fieldName -> Validator fieldName model
ifInvalidSlug getter fieldName =
    ElmValidate.ifFalse (\model -> Slug.isValid (getter model)) ( fieldName, "Slugs can contain only alphanumeric characteres and -." )


ifShorterThan : Int -> (model -> String) -> fieldName -> Validator fieldName model
ifShorterThan minValue getter fieldName =
    ElmValidate.ifTrue (\model -> String.length (getter model) < minValue) ( fieldName, "Is shorter than " ++ String.fromInt minValue ++ " characters" )

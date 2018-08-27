-- Do not manually edit this file, it was auto-generated by Graphqelm
-- https://github.com/dillonkearns/graphqelm


module Api.Object.Project exposing (..)

import Api.InputObject
import Api.Interface
import Api.Object
import Api.Scalar
import Api.Union
import Graphqelm.Field as Field exposing (Field)
import Graphqelm.Internal.Builder.Argument as Argument exposing (Argument)
import Graphqelm.Internal.Builder.Object as Object
import Graphqelm.Internal.Encode as Encode exposing (Value)
import Graphqelm.OptionalArgument exposing (OptionalArgument(Absent))
import Graphqelm.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


{-| Select fields to build up a SelectionSet for this object.
-}
selection : (a -> constructor) -> SelectionSet (a -> constructor) Api.Object.Project
selection constructor =
    Object.selection constructor


{-| -}
id : Field String Api.Object.Project
id =
    Object.fieldDecoder "id" [] Decode.string


{-| -}
name : Field String Api.Object.Project
name =
    Object.fieldDecoder "name" [] Decode.string


{-| -}
deploymentImage : Field String Api.Object.Project
deploymentImage =
    Object.fieldDecoder "deploymentImage" [] Decode.string


{-| -}
accessToken : Field String Api.Object.Project
accessToken =
    Object.fieldDecoder "accessToken" [] Decode.string


{-| -}
environments : SelectionSet decodesTo Api.Object.Environment -> Field (List decodesTo) Api.Object.Project
environments object =
    Object.selectionField "environments" [] object (identity >> Decode.list)


{-| -}
createdAt : Field Int Api.Object.Project
createdAt =
    Object.fieldDecoder "createdAt" [] Decode.int


{-| -}
updatedAt : Field Int Api.Object.Project
updatedAt =
    Object.fieldDecoder "updatedAt" [] Decode.int

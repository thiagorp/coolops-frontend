-- Do not manually edit this file, it was auto-generated by Graphqelm
-- https://github.com/dillonkearns/graphqelm


module Api.Object.Environment exposing (..)

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
selection : (a -> constructor) -> SelectionSet (a -> constructor) Api.Object.Environment
selection constructor =
    Object.selection constructor


{-| -}
id : Field String Api.Object.Environment
id =
    Object.fieldDecoder "id" [] Decode.string


{-| -}
name : Field String Api.Object.Environment
name =
    Object.fieldDecoder "name" [] Decode.string


{-| -}
environmentVariables : SelectionSet decodesTo Api.Object.Param -> Field (List decodesTo) Api.Object.Environment
environmentVariables object =
    Object.selectionField "environmentVariables" [] object (identity >> Decode.list)


{-| -}
lastDeployment : SelectionSet decodesTo Api.Object.Deployment -> Field (Maybe decodesTo) Api.Object.Environment
lastDeployment object =
    Object.selectionField "lastDeployment" [] object (identity >> Decode.nullable)


{-| -}
createdAt : Field Int Api.Object.Environment
createdAt =
    Object.fieldDecoder "createdAt" [] Decode.int


{-| -}
updatedAt : Field Int Api.Object.Environment
updatedAt =
    Object.fieldDecoder "updatedAt" [] Decode.int

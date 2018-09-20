module App.Forms.Environments.Data exposing
    ( Environment
    , Project
    , Response
    , query
    )

import Api
import Api.Object as Api
import Api.Object.Environment as EnvironmentApi
import Api.Object.Param as ParamApi
import Api.Object.Project as ProjectApi
import Api.Query as Query
import Dict exposing (Dict)
import Graphql.Field as Field exposing (Field)
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet exposing (SelectionSet, with)


type alias Response =
    List Project


type alias Param =
    { key : String
    , value : String
    }


type alias Environment =
    { id : String
    , name : String
    , environmentVars : Dict String String
    }


type alias Project =
    { name : String
    , environments : List Environment
    }


param : SelectionSet ( String, String ) Api.Param
param =
    ParamApi.selection (\a b -> ( a, b ))
        |> with ParamApi.key
        |> with ParamApi.value


environment : SelectionSet Environment Api.Environment
environment =
    EnvironmentApi.selection Environment
        |> with EnvironmentApi.id
        |> with EnvironmentApi.name
        |> with
            (EnvironmentApi.environmentVariables param
                |> Field.map Dict.fromList
            )


project : SelectionSet Project Api.Project
project =
    ProjectApi.selection Project
        |> with ProjectApi.name
        |> with (ProjectApi.environments environment)


query : Field Response RootQuery
query =
    Query.projects project

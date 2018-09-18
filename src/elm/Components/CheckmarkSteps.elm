module Components.CheckmarkSteps exposing (Step, Steps(..), view)

import Html exposing (..)
import Html.Attributes exposing (..)


type Steps
    = Steps (List Step) Step (List Step)


type alias Step =
    { text : String }


unfinishedStep : Step -> Html msg
unfinishedStep step =
    li [ class "checkmark-steps-row text-black-50" ]
        [ i [ class "far fa-circle text-muted" ] []
        , text step.text
        ]


currentStep : Step -> Html msg
currentStep step =
    li [ class "checkmark-steps-row text-black-50" ]
        [ i [ class "far fa-dot-circle text-primary" ] []
        , strong [] [ text step.text ]
        ]


finishedStep : Step -> Html msg
finishedStep step =
    li [ class "checkmark-steps-row text-black-50" ]
        [ i [ class "fas fa-check-circle text-success" ] []
        , text step.text
        ]


view : Steps -> Html msg
view (Steps finished current unfinished) =
    ul [ class "list-unstyled" ]
        (List.map finishedStep finished ++ [ currentStep current ] ++ List.map unfinishedStep unfinished)

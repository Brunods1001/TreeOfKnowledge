module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, li, text, ul)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL
-- The model saves question-answer tuples


type alias Node =
    { key : Int
    , value : Maybe String
    }


type alias Edge =
    { source : Node
    , sink : Node
    }


type alias QuestionAnswer =
    { question : Node
    , no : Node
    , yes : Node
    }


type alias Answer =
    { question : Node
    , response : String
    }



-- TREE
-- the tree is a list of QuestionAnswer objects


type alias Tree =
    List QuestionAnswer


type alias Model =
    { currentQuestion : Maybe QuestionAnswer
    , pastAnswers : List Answer
    , tree : Tree
    , result : Maybe String
    }


treeInit : Tree
treeInit =
    [ { question = { key = 1, value = Just "Are you ok?" }
      , no = { key = 2, value = Just "Are you from Brazil?" }
      , yes = { key = 3, value = Just "Are you not from Brazil?" }
      }
    , { question = { key = 2, value = Just "Are you from Brazil?" }
      , no = { key = 5, value = Just "You are fine!" }
      , yes = { key = 6, value = Just "You are not fine!" }
      }
    , { question = { key = 3, value = Just "Are you not from Brazil?" }
      , no = { key = 7, value = Just "You are fine!" }
      , yes = { key = 8, value = Just "You are not fine!" }
      }
    ]


getFirstQuestionFromTree : Tree -> Maybe QuestionAnswer
getFirstQuestionFromTree t =
    List.head t


init : Model
init =
    { currentQuestion = getFirstQuestionFromTree treeInit
    , pastAnswers = []
    , tree = treeInit
    , result = Nothing
    }



-- UPDATE


type Msg
    = Yes
    | No


update : Msg -> Model -> Model
update msg model =
    case model.currentQuestion of
        Just currentQuestion ->
            case msg of
                Yes ->
                    case grabQuestionFromTree model.tree currentQuestion.yes of
                        Just newQuestion ->
                            { currentQuestion = Just newQuestion
                            , pastAnswers = model.pastAnswers ++ [ { question = currentQuestion.question, response = "Yes" } ]
                            , tree = model.tree
                            , result = Nothing
                            }

                        Nothing ->
                            { currentQuestion = Nothing
                            , pastAnswers = model.pastAnswers ++ [ { question = currentQuestion.question, response = "Yes" } ]
                            , tree = model.tree
                            , result = currentQuestion.yes.value
                            }

                No ->
                    case grabQuestionFromTree model.tree currentQuestion.no of
                        Just newQuestion ->
                            { currentQuestion = Just newQuestion
                            , pastAnswers = model.pastAnswers ++ [ { question = currentQuestion.question, response = "No" } ]
                            , tree = model.tree
                            , result = Nothing
                            }

                        Nothing ->
                            { currentQuestion = Nothing
                            , pastAnswers = model.pastAnswers ++ [ { question = currentQuestion.question, response = "No" } ]
                            , tree = model.tree
                            , result = currentQuestion.no.value
                            }

        Nothing ->
            { model
                | result = Just "RESULT"
            }


grabQuestionFromTree : Tree -> Node -> Maybe QuestionAnswer
grabQuestionFromTree tree node =
    List.head (List.filter (\x -> x.question == node) tree)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewResult model
        , text ("Question: " ++ questionToString model)
        , button [ onClick Yes ] [ text "Yes" ]
        , button [ onClick No ] [ text "No" ]
        , viewPastAnswers model
        ]


viewResult : Model -> Html Msg
viewResult model =
    case model.result of
        Just result ->
            h1 [] [ text ("Result: " ++ result) ]

        Nothing ->
            text ""


questionToString : Model -> String
questionToString model =
    case model.currentQuestion of
        Just questionAnswer ->
            case questionAnswer.question.value of
                Just q ->
                    q

                Nothing ->
                    "no question found!"

        Nothing ->
            "No more questions!"


viewPastAnswers : Model -> Html Msg
viewPastAnswers model =
    ul [] (List.map viewAnswer model.pastAnswers)


viewAnswer : Answer -> Html Msg
viewAnswer answer =
    li [] [ text (nodeToString answer.question ++ ": " ++ answer.response) ]


viewNode : Node -> Html Msg
viewNode node =
    li [] [ text (nodeToString node) ]


nodeToString : Node -> String
nodeToString node =
    case node.value of
        Just value ->
            String.fromInt node.key ++ ": " ++ value

        Nothing ->
            String.fromInt node.key ++ ": " ++ "No question found!"

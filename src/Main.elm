-- Show a list of questions
-- Record answers
--
-- Dynamically show questions


module Main exposing (..)

import Array
import Browser
import Html exposing (Html, a, button, div, h2, h4, hr, img, li, ol, p, text, ul)
import Html.Attributes exposing (height, href, src, target, width)
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


type Msg
    = StartQuiz
    | AnswerYes
    | AnswerNo
    | SkipAnswer
    | NextQuestion
    | CheckResult
    | EndQuiz


type Answer
    = Yes
    | No
    | Unanswered


type alias Question =
    { text : String }


type alias QuestionDependencies =
    Array.Array QuestionDependency


type alias QuestionDependencyAnswer =
    { question_dependency : QuestionDependency
    , answer : Answer
    }


type alias QuestionDependency =
    { question : Question
    , id : Maybe Int
    }



-- A Rule is a set of keys
-- If a set of keys in the pastAnswers list is Yes, then a result is triggered


type alias Rule =
    { keys : List String
    }


type PageView
    = Home
    | Quiz
    | PostQuiz


type alias Model =
    { currentQuestion : Maybe QuestionDependency
    , questions : QuestionDependencies
    , pastAnswers : Array.Array QuestionDependencyAnswer
    , result : Maybe Bool
    , numQuestion : Int
    , ruleSet : List Rule
    , pageView : PageView
    }


questions_init : QuestionDependencies
questions_init =
    Array.fromList
        [ QuestionDependency (Question "Have you had vaginal or anal sex in the past 6 months?") Nothing
        , QuestionDependency (Question "IVDU?") Nothing
        , QuestionDependency (Question "Does their partner have HIV?") (Just 0)
        , QuestionDependency (Question "Did they have a bacterial STI recently?") (Just 0)
        , QuestionDependency (Question "Do they have a history of inconsistent or no condom use?") (Just 0)
        , QuestionDependency (Question "Does their partner have HIV?") (Just 1)
        ]


init : Model
init =
    { currentQuestion = Array.get 0 questions_init
    , questions = questions_init
    , pastAnswers = Array.fromList []
    , result = Nothing
    , numQuestion = 0
    , ruleSet = []
    , pageView = Home
    }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case model.currentQuestion of
        Just cq ->
            case msg of
                StartQuiz ->
                    { init
                        | pageView = Quiz
                    }

                AnswerYes ->
                    update CheckResult
                        { model
                            | pastAnswers = Array.append model.pastAnswers (Array.fromList [ QuestionDependencyAnswer cq Yes ])
                        }

                AnswerNo ->
                    update NextQuestion
                        { model
                            | pastAnswers = Array.append model.pastAnswers (Array.fromList [ QuestionDependencyAnswer cq No ])
                        }

                SkipAnswer ->
                    update NextQuestion
                        { model
                            | pastAnswers = Array.append model.pastAnswers (Array.fromList [ QuestionDependencyAnswer cq Unanswered ])
                        }

                CheckResult ->
                    let
                        maybe_result =
                            determineResult model
                    in
                    case maybe_result of
                        Just result ->
                            if result == True then
                                update EndQuiz model

                            else
                                update NextQuestion model

                        Nothing ->
                            update NextQuestion model

                NextQuestion ->
                    case getNextQuestion model of
                        Just next_q ->
                            case next_q.id of
                                Just id ->
                                    if checkIfAnswerIsYesById id model.pastAnswers then
                                        { model
                                            | currentQuestion = Just next_q
                                            , numQuestion = model.numQuestion + 1
                                        }

                                    else
                                        -- Go to next question and check
                                        update SkipAnswer
                                            { model
                                                | currentQuestion = Just next_q
                                                , numQuestion = model.numQuestion + 1
                                            }

                                Nothing ->
                                    { model
                                        | currentQuestion = Just next_q
                                        , numQuestion = model.numQuestion + 1
                                    }

                        Nothing ->
                            { model
                                | result = determineResult model
                                , currentQuestion = Nothing
                                , pageView = PostQuiz
                            }

                EndQuiz ->
                    { model
                        | currentQuestion = Nothing
                        , result = determineResult model
                        , pageView = PostQuiz
                    }

        Nothing ->
            case msg of
                StartQuiz ->
                    { init | pageView = Quiz }

                _ ->
                    { model
                        | result = determineResult model
                        , pageView = PostQuiz
                    }


getNextQuestion : Model -> Maybe QuestionDependency
getNextQuestion model =
    Array.get (model.numQuestion + 1) model.questions


does_not_meet : String
does_not_meet =
    "This patient does not meet HIV PrEP eligibility."


meets : String
meets =
    "This patient is eligible for PrEP."


determineResult : Model -> Maybe Bool
determineResult model =
    let
        maybe_q1 =
            Array.get 0 model.pastAnswers

        maybe_q2 =
            Array.get 1 model.pastAnswers

        maybe_q3 =
            Array.get 2 model.pastAnswers

        maybe_q4 =
            Array.get 3 model.pastAnswers

        maybe_q5 =
            Array.get 4 model.pastAnswers

        maybe_q6 =
            Array.get 5 model.pastAnswers
    in
    case [ maybe_q1, maybe_q2, maybe_q3, maybe_q4, maybe_q5, maybe_q6 ] of
        [ Just q1, Just q2, Nothing, Nothing, Nothing, Nothing ] ->
            if q1.answer == Yes && q2.answer == Yes then
                Just True

            else
                Just False

        [ Just q1, Just q2, Just q3, Just q4, Just q5, Just q6 ] ->
            if q1.answer == No && q2.answer == No then
                Just False

            else if q1.answer == Yes && q2.answer == Yes then
                Just True

            else if q1.answer == Yes && (q3.answer == Yes || q4.answer == Yes || q5.answer == Yes) then
                Just True

            else if q2.answer == Yes && q6.answer == Yes then
                Just True

            else
                Just False

        _ ->
            Nothing


viewResult : Model -> Html Msg
viewResult model =
    let
        maybe_res =
            determineResult model
    in
    case maybe_res of
        Just res ->
            if res then
                viewPrEPInfoPage

            else
                viewDoesNotMeetPrEPPage

        Nothing ->
            div []
                [ text "There was an error" ]


filterYes : List QuestionDependencyAnswer -> List QuestionDependency
filterYes question_answers =
    -- List.map (\x -> x.question) question_answers
    question_answers |> List.filter (\x -> x.answer == Yes) |> List.map (\x -> x.question_dependency)


viewYesResponses : Model -> Html Msg
viewYesResponses model =
    div []
        (model.pastAnswers |> Array.toList |> filterYes |> List.map viewQuestionDependency)


viewAllResponses : Model -> Html Msg
viewAllResponses model =
    div []
        (model.pastAnswers |> Array.toList |> List.map (\x -> x.question_dependency) |> List.map viewQuestionDependency)



-- If there is a QDA and it is Yes, return True, else return False


checkIfAnswerIsYesById : Int -> Array.Array QuestionDependencyAnswer -> Bool
checkIfAnswerIsYesById id arr =
    let
        maybe_qda =
            Array.get id arr
    in
    case maybe_qda of
        Just qda ->
            qda.answer == Yes

        Nothing ->
            False



-- VIEW


view : Model -> Html Msg
view model =
    case model.pageView of
        Home ->
            viewHome model

        Quiz ->
            viewQuiz model

        PostQuiz ->
            viewPostQuiz model



-- Start quiz


viewHome : Model -> Html Msg
viewHome _ =
    div []
        [ startQuizButton "Start"
        ]


viewQuiz : Model -> Html Msg
viewQuiz model =
    div []
        [ div []
            [ viewCurrentQuestion model ]
        ]


showHIVScreen : Html Msg
showHIVScreen =
    div []
        [ h2 [] [ text "Guidelines for HIV screening:" ]
        , ul []
            [ li [] [ text "Frequency" ]
            , ul []
                [ li [] [ text "Age 13-64 years old at least once" ]
                , li [] [ text "At least once annually:" ]
                , ul []
                    [ li [] [ text "MSM (gay/bi/other)" ]
                    , li [] [ text "IVDU and their sex partners" ]
                    , li [] [ text "sex in exchange for goods/services" ]
                    , li [] [ text "sex partner has HIV (regardless of viral load)" ]
                    , li [] [ text "self/partner had >=1 partner since their most recent HIV test" ]
                    , li [] [ text "receiving treatment for hepatitis, TB, or STI" ]
                    ]
                ]
            , li [] [ text "Test" ]
            , ul []
                [ li [] [ text "RNA PCR (viral load) for acute infections before antibodies become positive" ]
                , li [] [ text "p24Ag/Ab" ]
                , li [] [ text "rapid test if urgent/more practical; need to confirm with p24Ag/Ab due to lower sensitivities and specificities (varies by brand of rapid test)" ]
                ]
            ]
        ]


viewHIVScreen : Model -> Html Msg
viewHIVScreen _ =
    showHIVScreen


viewPostQuiz : Model -> Html Msg
viewPostQuiz model =
    div []
        [ startQuizButton "Retake test"
        , a [ target "_", href "https://hivrisk.cdc.gov/risk-estimator-tool/#-mb|rvi.stdn.stdp" ] [ text "See the CDC's risk estimator tool for more information" ]
        , hr [] []
        , showHIVScreen
        , showResult model
        ]



-- info page


viewDoesNotMeetPrEPPage : Html Msg
viewDoesNotMeetPrEPPage =
    div [] [ text does_not_meet ]


viewPrEPInfoPage : Html Msg
viewPrEPInfoPage =
    div []
        [ h2 [] [ text "Guidelines for offering HIV PrEP:" ]
        , ol []
            [ li []
                [ h4 [] [ text "This patient is eligible for PrEP" ]
                , p [] [ text "Prior to initiation of PrEP, the patient should have the following tests:" ]
                , ul []
                    [ li [] [ text "HIV Ab/Ag test to ensure HIV negative" ]
                    , ul []
                        [ li [] [ text "If there's a concern for exposure in the past 4 weeks, include a test for RNA (viral load)." ] ]
                    , li [] [ text "BMP (for eGFR)" ]
                    , li [] [ text "HBV" ]
                    , li [] [ text "pregnancy" ]
                    ]
                ]
            , li []
                [ h4 [] [ text "Available regimens:" ]
                , img [ src "../table.png", width 800, height 800 ] []
                ]
            , li []
                [ h4 [] [ text "Refer to infectious diseases for monitoring and refills. Regardless of regimen, the patient will need to follow up every 3 months" ]
                ]
            ]
        ]



-- view result


showResult : Model -> Html Msg
showResult model =
    case model.result of
        Just _ ->
            viewResult model

        Nothing ->
            div [] [ text "Error in getting result!" ]



-- view questions


viewCurrentQuestion : Model -> Html Msg
viewCurrentQuestion model =
    case model.currentQuestion of
        Just cQ ->
            viewQuestionButton cQ.question

        Nothing ->
            showResult model


getQuestion : Int -> QuestionDependencies -> Maybe QuestionDependency
getQuestion key question_dependencies =
    Array.get key question_dependencies


toStringQuestionDependencyId : QuestionDependency -> String
toStringQuestionDependencyId questionD =
    case questionD.id of
        Just id ->
            String.fromInt id

        Nothing ->
            "\nNO ID"


viewQuestionDependency : QuestionDependency -> Html Msg
viewQuestionDependency questionD =
    div []
        [ text ("Question dependency: " ++ questionD.question.text)
        , text (toStringQuestionDependencyId questionD)
        ]


viewQuestion : Question -> Html Msg
viewQuestion question =
    div []
        [ text ("Question: " ++ question.text)
        ]


startQuizButton : String -> Html Msg
startQuizButton txt =
    div []
        [ button [ onClick StartQuiz ] [ text txt ]
        ]


viewQuestionButton : Question -> Html Msg
viewQuestionButton question =
    div []
        [ viewQuestion question
        , questionButton
        ]


questionButton : Html Msg
questionButton =
    div []
        [ button [ onClick AnswerYes ] [ text "Yes" ]
        , button [ onClick AnswerNo ] [ text "No" ]
        ]



-- view answers


viewQuestionAnswers : Model -> List (Html Msg)
viewQuestionAnswers model =
    Array.toList
        (Array.map viewQuestionAnswer (Array.filter (\x -> x.answer /= Unanswered) model.pastAnswers))


viewQuestionAnswer : QuestionDependencyAnswer -> Html Msg
viewQuestionAnswer qa =
    div []
        [ p [] [ text qa.question_dependency.question.text ]
        , viewAnswer qa.answer
        ]


viewAnswer : Answer -> Html Msg
viewAnswer answer =
    case answer of
        Yes ->
            div [] [ p [] [ text "YES" ] ]

        No ->
            div [] [ p [] [ text "NO" ] ]

        Unanswered ->
            div [] [ p [] [ text "UNANSWERED" ] ]

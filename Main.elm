module Main exposing (..)
import Browser
import Html exposing (Html, button, br, div, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes as Attr exposing (type_)
import SimultaneousEquation exposing (..)
import Random exposing (Generator, generate)

type alias Model =
  { matrix : Matrix Int
  , values : Vec2 Int
  , isReady2view : Bool
  , viewAnswer : Bool
  , numOfProblems : Int
  , answerList : List (Vec2 Fraction)
  , maxProblems : Int
  , isFirstState : Bool
  }

initModel =
  { matrix = ((0,0),(0,0))
  , values = (0,0)
  , isReady2view = False
  , viewAnswer = False
  , numOfProblems = 0
  , answerList = []
  , maxProblems = 10
  , isFirstState = True
  }

view model =
  if model.isFirstState
  then
    div []
      [ Html.text "input number of questions below..."
      , div []
          [ input
            [ type_ "number"
            , onInput OnInput
            , Attr.max "100"
            , Attr.min "1"
            , Attr.value (i2s model.maxProblems)
            ][]
          ]
      , button [onClick GoNext][Html.text "Next"]
      ]
  else
    if model.isReady2view
    then
      if model.viewAnswer
      then
        div []
          [ Html.text "Answer"
          , div []
              ( model.answerList
                  |> List.indexedMap
                    (\n (a,b) ->
                      (i2s (n+1) ++ ". x=" ++ fraction2String a ++ " ,y=" ++ fraction2String b)
                        |> Html.text
                        |> List.singleton
                        |> div []
                    )
              )
          , button [ onClick GoNext ][ Html.text "Continue" ]
          ]
      else
        div []
          [ ("Question " ++ i2s model.numOfProblems ++ ".")
              |> Html.text
          , div []
            ( viewEquation model.matrix model.values
              |> List.map (\s -> div [][Html.text s])
            )
          , button
            [ onClick
              ( if model.numOfProblems /= model.maxProblems
                then GoNext
                else ViewAnswer
              )
            ]
            [ Html.text
              ( if model.numOfProblems /= model.maxProblems
                then "Next"
                else "ViewAnswer"
              )
            ]
          ]
    else
      Html.text "generating..."

type Msg
  = GetValues (Matrix Int, Vec2 Int)
  | ViewAnswer
  | GoNext
  | OnInput String


update msg model =
  case msg of
    GetValues (mat, v) ->
      if determinate mat == 0
      then
        ( model
        , generate GetValues (Random.pair randMatrix randVec2)
        )
      else
        ( { model
          | matrix = mat
          , values = v
          , answerList = model.answerList ++ [cramersRule mat v]
          , isReady2view = True
          }
        , Cmd.none
        )

    ViewAnswer ->
      ( { model | viewAnswer = True }, Cmd.none )

    GoNext ->
      ( { model
        | numOfProblems = modBy model.maxProblems model.numOfProblems + 1
        , isReady2view = False
        , viewAnswer = False
        , answerList =
            if model.numOfProblems == model.maxProblems
            then []
            else model.answerList
        , isFirstState = False
        }
      , generate GetValues (Random.pair randMatrix randVec2)
      )

    OnInput s ->
      ( { model
        | maxProblems = Maybe.withDefault 0 (String.toInt s)
        }
      , Cmd.none
      )

main : Program () Model Msg
main =
  Browser.element
    { init =
        \_ ->
          ( initModel, Cmd.none )
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }

randMatrix : Generator (Matrix Int)
randMatrix =
  Random.pair randVec2 randVec2

randVec2 : Generator (Vec2 Int)
randVec2 =
  Random.pair randint randint

randint : Generator Int
randint =
  let
    rangeMin = -15
    rangeMax = 15
  in
    Random.int rangeMin rangeMax
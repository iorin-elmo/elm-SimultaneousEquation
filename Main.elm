module Main exposing (..)
import Browser
import Html exposing (Html, button, br, div)
import Html.Events exposing (onClick)
import SimultaneousEquation exposing (..)
import Random exposing (Generator, generate)

type alias Model =
  { matrix : Matrix Int
  , values : Vec2 Int
  , isReady2view : Bool
  , viewAnswer : Bool
  , numOfProblems : Int
  , answerList : List (Vec2 Fraction)
  }

initModel =
  { matrix = ((0,0),(0,0))
  , values = (0,0)
  , isReady2view = False
  , viewAnswer = False
  , numOfProblems = 1
  , answerList = []
  }

view model =
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
            ( if model.numOfProblems /= 10
              then GoNext
              else ViewAnswer
            )
          ]
          [ Html.text
            ( if model.numOfProblems /= 10
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
        | numOfProblems = modBy 10 model.numOfProblems + 1
        , isReady2view = False
        , viewAnswer = False
        , answerList =
            if model.numOfProblems == 10
            then []
            else model.answerList
        }
      , generate GetValues (Random.pair randMatrix randVec2)
      )

main : Program () Model Msg
main =
  Browser.element
    { init =
        \_ ->
          ( initModel
          , generate GetValues (Random.pair randMatrix randVec2)
          )
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
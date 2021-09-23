module Main exposing (..)
import Browser
import Html exposing (Html, button, br, div)
import Html.Events exposing (onClick)
import SimultaneousEquation exposing (..)
import Random exposing (Generator, generate)

type alias Model =
  { matrix : Matrix Int
  , values : Vec2 Int
  , ans : Vec2 Fraction
  , isReady2view : Bool
  , viewAnswer : Bool
  }

initModel =
  { matrix = ((0,0),(0,0))
  , values = (0,0)
  , ans = ((0,0),(0,0))
  , isReady2view = False
  , viewAnswer = False
  }

view model =
  if model.isReady2view
  then
    div []
      <| List.append
        ( viewEquation model.matrix model.values
          |> List.map Html.text
          |> List.intersperse (br[][])
        )
        [ br[][]
        , if model.viewAnswer
          then
            (\(a,b) ->
              ("Answer : x=" ++ fraction2String a ++ " ,y=" ++ fraction2String b)
                |> Html.text
            ) model.ans
          else Html.text ""
        , br[][]
        , button
          [ onClick
            ( if model.viewAnswer
              then GoNext
              else ViewAnswer
            )
          ]
          [ Html.text
            ( if model.viewAnswer
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
          , ans = cramersRule mat v
          , isReady2view = True
          }
        , Cmd.none
        )

    ViewAnswer ->
      ( { model | viewAnswer = True }, Cmd.none )

    GoNext ->
      ( { model
        | viewAnswer = False
        , isReady2view = False
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
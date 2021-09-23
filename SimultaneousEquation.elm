module SimultaneousEquation exposing (..)
import Html exposing (Html)

type alias Fraction = (Int, Int)
type alias Vec2 a = (a, a)
type alias Matrix a = (Vec2 a, Vec2 a)

cramersRule : Matrix Int -> Vec2 Int -> Vec2 Fraction
cramersRule (a1, a2) v =
  (determinate (v, a2), determinate (a1, v))
    |> mapAll
      (\p ->
        commonDivide
          ( p
          , ( determinate ( a1, a2 ))
          )
      )

mapAll : (a -> b) -> (a,a) -> (b,b)
mapAll f (v1,v2) = (f v1, f v2)

determinate ((a1, a2), (b1, b2)) = a1*b2-a2*b1

commonDivide : Fraction -> Fraction
commonDivide (a,b) =
  if b < 0
  then commonDivide (-a,-b)
  else
    (a,b)
      |> mapAll (\p -> p // gcd (abs a) (abs b))

gcd : Int -> Int -> Int
gcd a b =
  if a<b
  then gcd b a
  else
    if b==0
    then a
    else gcd b (modBy b a)

viewEquation : Matrix Int -> Vec2 Int -> List String
viewEquation ((x1,x2),(y1,y2)) (v1,v2) =
  [ viewEquationHelper x1 y1 v1
  , viewEquationHelper x2 y2 v2
  ]

i2s = String.fromInt

viewEquationHelper x y v =
  if x==0
  then i2s y ++ "y=" ++ i2s v
  else
    if y==0
    then i2s x ++ "x=" ++ i2s v
    else
      if y<0
      then
        i2s x ++ "x" ++ i2s y ++ "y=" ++ i2s v
      else
        i2s x ++ "x+" ++ i2s y ++ "y=" ++ i2s v

fraction2String : Fraction -> String
fraction2String (a,b) =
  if b==1
  then i2s a
  else i2s a ++ "/" ++ i2s b
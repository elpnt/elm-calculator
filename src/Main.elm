import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



-- MAIN


main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }



-- MODEL


type alias Calculator =
  { add : Float -> Float -> Float
  , sub : Float -> Float -> Float
  , mul : Float -> Float -> Float
  , div : Float -> Float -> Float
  }


calculator : Calculator
calculator =
  { add = (\x y -> x + y)
  , sub = (\x y -> x - y)
  , mul = (\x y -> x * y)
  , div = (\x y -> x / y)
  }


type alias Model =
  { display : String
  , function : Float -> Float -> Float
  , lastValue : Float
  , append : Bool
  }


init : Model
init =
  { display = "0"
  , function = (\x y -> y)
  , lastValue = 0
  , append = True
  }

parseFloat : String -> Float
parseFloat input =
  Maybe.withDefault 0 (String.toFloat input)


operation : Model -> (Float -> Float -> Float) -> Model
operation model function =
  { model | function = function
          , lastValue = (parseFloat model.display)
          , append = False
  }



-- UPDATE


type Msg
  = NoOp
  | Clear
  | Number Int
  | Zero
  | Decimal
  | Add
  | Subtract
  | Multiply
  | Divide
  | Enter
  | ChangeSign
  | Percent


update : Msg -> Model -> Model
update msg model =
  case msg of
    NoOp ->
      model

    Clear ->
      init

    Number int ->
      updateDisplay model int

    Decimal ->
      decimal model

    Zero ->
      zero model

    Add ->
      operation model calculator.add

    Subtract ->
      operation model calculator.sub

    Multiply ->
      operation model calculator.mul

    Divide ->
      operation model calculator.div

    Enter ->
      enter model

    ChangeSign ->
      changesign model

    Percent ->
      percent model


updateDisplay : Model -> Int -> Model
updateDisplay model int =
  if model.display /= "0" && model.append then
    { model | display = model.display ++ String.fromInt (int) }
  else
    { model | display = String.fromInt int
            , append = True
    }


zero : Model -> Model
zero model =
  if model.display == "0" || not model.append then
    { model | display = "0"
            , append = False
    }
  else
    { model | display = model.display ++ "0" }


decimal : Model -> Model
decimal model =
  if model.display /= "0" && model.append then
    { model | display = appendDecimal model.display }
  else
    { model | display = "0"
            , append = True
    }


appendDecimal : String -> String
appendDecimal str =
  if String.contains "." str then
    str
  else
    str ++ "."


enter : Model -> Model
enter model =
  if model.append then
    { model | display = calculate model
            , lastValue = parseFloat model.display
            , append = False
    }
  else
    { model | display = calculate model
            , append = False
    }


calculate : Model -> String
calculate model =
  model.function model.lastValue (parseFloat model.display) |> String.fromFloat


changesign : Model -> Model
changesign model =
  let
    num = parseFloat model.display
  in
    { model | display = String.fromFloat (-num) }


percent : Model -> Model
percent model =
  let
    num = (parseFloat model.display) / 100.0
  in
    { model | display = String.fromFloat num }


-- VIEW


createButton : Msg -> String -> Html Msg
createButton msg buttonText =
  td []
     [ button
         [ onClick msg ]
         [ text buttonText ]
     ]


view : Model -> Html Msg
view model =
  table
    []
    [ tr []
         [ td [ colspan 4 ]
              [ text model.display ]
         ]
    , tr []
         [ createButton Clear "C"
         , createButton ChangeSign "+/-"
         , createButton Percent "%"
         , createButton Divide "/"
         ]
    , tr []
         [ createButton (Number 7) "7"
         , createButton (Number 8) "8"
         , createButton (Number 9) "9"
         , createButton Multiply "x"
         ]
    , tr []
         [ createButton (Number 4) "4"
         , createButton (Number 5) "5"
         , createButton (Number 6) "6"
         , createButton Subtract "-"
         ]
    , tr []
         [ createButton (Number 1) "1"
         , createButton (Number 2) "2"
         , createButton (Number 3) "3"
         , createButton Add "+"
         ]
    , tr []
         [ td [ colspan 2 ]
              [ button [ onClick Zero ] [ text "0" ] ]
         , createButton Decimal "."
         , createButton Enter "="
         ]
    ]

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

type Operator
  = OpAdd
  | OpSubtract
  | OpMultiply
  | OpDivide
  | OpNone


type alias Model =
  { displaynum : String
  , memorynum : String
  , memoryop : Operator
  }


init : Model
init =
  { displaynum = "0"
  , memorynum = "0"
  , memoryop = OpNone
  }



-- UPDATE


type Msg
  = NoOp
  | Num Int
  | ChangeSign
  | Percent
  | Add
  | Subtract
  | Multiply
  | Divide
  | Enter


update : Msg -> Model -> Model
update msg model =
  case msg of
    NoOp ->
      model

    Num int ->
      let
          str = String.fromInt int
      in
          { model | displaynum = model.displaynum ++ str }

    ChangeSign ->
      { model | displaynum =
                  case String.toFloat model.displaynum of
                    Err errmsg ->
                      "0"

                    Ok num ->
                      String.fromFloat -num
      }


    Percent ->
      { model | displaynum =
                  case String.toFloat model.displaynum of
                    Err err ->
                      model.displaynum

                    Ok num ->
                      String.fromFloat (num/100.0)
                      }

    Add ->
      { model | memorynum = model.displaynum
              , memoryop = OpAdd
      }

    Subtract ->
      { model | memorynum = model.displaynum
              , memoryop = OpSubtract
      }

    Multiply ->
      { model | memorynum = model.displaynum
              , memoryop = OpMultiply
      }

    Divide ->
      { model | memorynum = model.displaynum
              , memoryop = OpDivide
      }

    Enter ->
      case model.memoryop of
        OpAdd ->
          { model | displaynum =
                      String.fromFloat
                        <| str2float model.memorynum + str2float model.memorynum
                  , memorynum = "0"
          }

        OpSubtract ->
          { model | displaynum =
                      String.fromFloat
                        <| str2float model.memorynum - str2float model.displaynum
                  , memorynum = "0"
          }

        OpMultiply ->
          { model | displaynum =
                      String.fromFloat
                        <| str2float model.memorynum * str2float model.displaynum
                  , memorynum = "0"
          }

        OpDivide ->
          { model | displaynum =
                      case str2int model.displaynum of
                        0 ->
                          "Error"
                        _ ->
                          String.fromFloat
                            <| str2float model.memorynum / str2float model.displaynum
                  , memorynum = "0"
          }

        OpNone ->
          model


str2float : String -> Float
str2float str =
  Maybe.withDefault 1 (String.toFloat str)


str2int : String -> Int
str2int str =
  Maybe.withDefault 1 (String.toInt str)



-- VIEW

view : Model -> Html Msg
view model =
  table
    []
    [ tr
        []
        [ td [ colspan 3 ]
             [ text "hello!" ]
        ]
    , tr
        []
        [ td [] [ button [ class "other"
                         , onClick ] [ text "C" ] ]
        , td [] [ button [ class "other" ] [ text "+/-" ] ]
        , td [] [ button [ class "other" ] [ text "%" ] ]
        , td [] [ button [ class "operator" ] [ text "/" ] ]
        ]
    , tr
        []
        [ td [] [ button [ class "number" ] [ text "7" ] ]
        , td [] [ button [ class "number" ] [ text "8" ] ]
        , td [] [ button [ class "number" ] [ text "9" ] ]
        , td [] [ button [ class "operator" ] [ text "x" ] ]
        ]
    , tr
        []
        [ td [] [ button [ class "number" ] [ text "4" ] ]
        , td [] [ button [ class "number" ] [ text "5" ] ]
        , td [] [ button [ class "number" ] [ text "6" ] ]
        , td [] [ button [ class "operator" ] [ text "-" ] ]
        ]
    , tr
        []
        [ td [] [ button [ class "number" ] [ text "1" ] ]
        , td [] [ button [ class "number" ] [ text "2" ] ]
        , td [] [ button [ class "number" ] [ text "3" ] ]
        , td [] [ button [ class "operator" ] [ text "+" ] ]
        ]
    , tr
        []
        [ td [ colspan 2 ]
             [ button [ class "number" ] [ text "0" ] ]
        , td [] [ button [ class "number" ] [ text "." ] ]
        , td [] [ button [ class "operator" ] [ text "=" ] ]
        ]
    ]

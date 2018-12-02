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
          case model.displaynum of
            "0" ->
              { model | displaynum = str }
            _ ->
              { model | displaynum = model.displaynum ++ str }

    ChangeSign ->
      model


    Percent ->
      model

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
             [ text model.displaynum ]
        ]
    , tr
        []
        [ td [] [ button [ class "other" ] [ text "C" ] ]
        , td [] [ button [ class "other" ] [ text "+/-" ] ]
        , td [] [ button [ class "other" ] [ text "%" ] ]
        , td [] [ button [ class "operator" ] [ text "/" ] ]
        ]
    , tr
        []
        [ td [] [ createNumButton 7 ]
        , td [] [ createNumButton 8 ]
        , td [] [ createNumButton 9 ]
        , td [] [ button [ class "operator" ] [ text "x" ] ]
        ]
    , tr
        []
        [ td [] [ createNumButton 4 ]
        , td [] [ createNumButton 5 ]
        , td [] [ createNumButton 6 ]
        , td [] [ button [ class "operator" ] [ text "-" ] ]
        ]
    , tr
        []
        [ td [] [ createNumButton 1 ]
        , td [] [ createNumButton 2 ]
        , td [] [ createNumButton 3 ]
        , td [] [ button [ class "operator" ] [ text "+" ] ]
        ]
    , tr
        []
        [ td [ colspan 2 ] [ createNumButton 0 ]
        , td [] [ button [ class "number" ] [ text "." ] ]
        , td [] [ button [ class "operator" ] [ text "=" ] ]
        ]
    ]

createNumButton : Int -> Html Msg
createNumButton int =
  button [ class "number"
         , onClick (Num int)
         ]
         [ text (String.fromInt int) ]

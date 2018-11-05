module Main exposing (Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Events exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type Msg
    = AddInstruction
    | Execute


type Instruction
    = Instruction


type alias Model =
    { world : Result String World
    , instructions : List Instruction
    , executions : Int
    }


type alias World =
    Int


init : Model
init =
    { world = Ok 0
    , instructions = []
    , executions = 0
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddInstruction ->
            { model | instructions = Instruction :: model.instructions }

        Execute ->
            executeInstructions { model | executions = 0 }


executeInstructions : Model -> Model
executeInstructions model =
    case model.instructions of
        head :: tail ->
            let
                newModel =
                    { model | executions = model.executions + 1 }
            in
            case
                model.world
                    |> checkLimit newModel
                    |> executeInstruction head
            of
                Ok newWorld ->
                    executeInstructions
                        { newModel
                            | world = Ok newWorld
                            , instructions = tail
                        }

                Err reason ->
                    { model | world = Err reason, instructions = [] }

        [] ->
            model


checkLimit : Model -> Result String World -> Result String World
checkLimit model _ =
    if model.executions > 10 then
        Err "Limit of executions reached"

    else
        model.world


executeInstruction : Instruction -> Result String World -> Result String World
executeInstruction instruction world =
    world


view : Model -> Html Msg
view model =
    div []
        [ model
            |> Debug.toString
            |> text
        , p []
            [ button [ onClick AddInstruction ] [ text "Add" ]
            , button [ onClick Execute ] [ text "Execute" ]
            ]
        ]

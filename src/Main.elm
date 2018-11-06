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
    }


type alias World =
    Int


init : Model
init =
    { world = Ok 0
    , instructions = []
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddInstruction ->
            { model | instructions = Instruction :: model.instructions }

        Execute ->
            executeInstructions { model | world = Ok 0 }


executeInstructions : Model -> Model
executeInstructions model =
    case model.instructions of
        [] ->
            model

        instruction :: instructions ->
            case
                model.world
                    |> executeInstruction instruction
                    |> increaseExecutions
                    |> checkExecutionsLimit
            of
                Ok newWorld ->
                    executeInstructions
                        { model
                            | world = Ok newWorld
                            , instructions = instructions
                        }

                Err reason ->
                    { model | world = Err reason, instructions = [] }


executeInstruction : Instruction -> Result String World -> Result String World
executeInstruction instruction world =
    world


checkExecutionsLimit : Result String World -> Result String World
checkExecutionsLimit world =
    case world of
        Ok executions ->
            if executions > 3 then
                Err "Limit of executions reached"

            else
                world

        err ->
            err


increaseExecutions : Result String World -> Result String World
increaseExecutions world =
    world |> Result.map (\executions -> executions + 1)


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

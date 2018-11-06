module Main exposing (Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Events exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type Msg
    = AddInstruction Instruction
    | Execute


type Instruction
    = AddOne
    | RemoveOne
    | Block (List Instruction)


type alias Model =
    { world : Result String World
    , instructions : List Instruction
    }


type alias World =
    { executions : Int, somevar : Int }


init : Model
init =
    { world = Ok (World 0 0)
    , instructions = []
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddInstruction instruction ->
            { model | instructions = List.append model.instructions [ instruction ] }

        Execute ->
            { model | world = executeInstructions model.instructions model.world }


executeInstructions : List Instruction -> Result String World -> Result String World
executeInstructions instructions world =
    case instructions of
        [] ->
            world

        instruction :: rest ->
            case
                world
                    |> executeInstruction instruction
                    |> increaseExecutions
                    |> checkExecutionsLimit
            of
                Ok newWorld ->
                    executeInstructions rest (Ok newWorld)

                Err reason ->
                    Err reason


executeInstruction : Instruction -> Result String World -> Result String World
executeInstruction instruction maybeWorld =
    case maybeWorld of
        Ok world ->
            case instruction of
                AddOne ->
                    Ok { world | somevar = world.somevar + 1 }

                RemoveOne ->
                    Ok { world | somevar = world.somevar - 1 }

                Block instructions ->
                    executeInstructions instructions (Ok world)

        Err reason ->
            Err reason


executionsLimit : Int
executionsLimit =
    1000


checkExecutionsLimit : Result String World -> Result String World
checkExecutionsLimit world =
    case world of
        Ok { executions } ->
            if executions >= executionsLimit then
                Err "Limit of executions reached"

            else
                world

        err ->
            err


increaseExecutions : Result String World -> Result String World
increaseExecutions =
    Result.map (\world -> { world | executions = world.executions + 1 })


view : Model -> Html Msg
view model =
    div []
        [ model
            |> Debug.toString
            |> text
        , p []
            [ button [ onClick (AddInstruction AddOne) ] [ text "+" ]
            , button [ onClick (AddInstruction RemoveOne) ] [ text "-" ]
            , button [ onClick (AddInstruction (Block [ AddOne, AddOne, AddOne, AddOne, AddOne ])) ] [ text "+5" ]
            , button [ onClick Execute ] [ text "Execute" ]
            ]
        ]

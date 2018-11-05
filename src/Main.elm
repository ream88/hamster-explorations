module Main exposing (Msg(..), init, main, update, view)

import Browser
import Html exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type Msg
    = Msg


type alias Model =
    ()


init : Model
init =
    ()


update : Msg -> Model -> Model
update msg model =
    model


view : Model -> Html Msg
view model =
    model
        |> Debug.toString
        |> text

import Html.App
import Model exposing (..)
import View exposing (..)
import AI exposing (..)


main : Program Never
main = Html.App.beginnerProgram {
        model = init,
        view = view,
        update = update
    }


init : Model
init = Playing emptyBoard


update : Msg -> Model -> Model
update msg model =
    case (msg, model) of
        (Restart, _) ->
            init
        (CellClick pos, Playing board) ->
            placeMark board pos
            |> doAiTurn
        _ ->
            model


doAiTurn : Model -> Model
doAiTurn model =
    case model of
        Playing board ->
            let
                aiCell = play (Playing board)
            in
                placeMark board aiCell
        _ ->
            model


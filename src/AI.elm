module AI exposing (play)

import Model exposing (..)
import Util exposing (..)


type Result
    = Victory
    | Draw
    | Defeat


play : Model -> Int
play board =
    let 
        (pos, result) = search board
    in
        pos


search : Model -> (Int, Result)
search model =
    case model of
        GameOver board Nothing ->
            (0, Draw)
        GameOver board (Just player) ->
            if board.turn /= player
            then (0, Victory)
            else (0, Defeat)
        Playing board ->
            let
                results =
                    range 0 8
                    |> List.map (\pos -> (pos, placeMark board pos))
                    |> List.filter (\(pos, model) -> (getBoard model).turn /= board.turn)
                    |> List.map (\(pos, model) -> (pos, search model))
                    |> List.map (\(pos, (_, result)) -> (pos, invert result))
            in
                pickTurn results


invert : Result -> Result
invert res =
    case res of
        Victory -> Defeat
        Draw -> Draw
        Defeat -> Victory


pickTurn : List (Int, Result) -> (Int, Result)
pickTurn choices =
    let
        priority (_, res) =
            case res of
                Victory -> 2
                Draw -> 1
                Defeat -> 0
    in
        choices
        |> List.sortBy priority
        |> List.head
        |> Maybe.withDefault (0, Defeat)

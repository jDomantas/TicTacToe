module Model exposing (..)

import Util exposing (..)


type Player
    = Cross
    | Zero


type Cell
    = Empty
    | Taken Player


type alias Board =
    { cells: List Cell
    , turn: Player
    }


type Model
    = Playing Board
    | GameOver Board (Maybe Player)


type Msg
    = CellClick Int
    | Restart


emptyBoard : Board
emptyBoard = {
        cells = List.repeat 9 Empty,
        turn = Cross
    }


placeMark : Board -> Int -> Model
placeMark board position =
    let item = getElement position board.cells
    in case item of
        Just Empty ->
            let 
                newBoard = 
                    { cells = setElement position (Taken board.turn) board.cells
                    , turn = otherPlayer board.turn
                    }
            in 
                case getWinner newBoard of
                    Just player ->
                        GameOver newBoard (Just player)
                    Nothing ->
                        if isBoardFull newBoard
                        then GameOver newBoard Nothing
                        else Playing newBoard
        _ ->
            Playing board


isBoardFull : Board -> Bool
isBoardFull board = List.all (\c -> c /= Empty) board.cells


checkWin : Board -> List Int -> Maybe Player
checkWin board cells =
    let
        marks = List.map (\i -> Maybe.withDefault Empty (getElement i board.cells)) cells
    in
        case marks of
            [] ->
                Nothing
            Empty :: xs ->
                Nothing
            Taken player :: xs ->
                if List.all (\c -> c == Taken player) xs
                then Just player
                else Nothing


getWinner : Board -> Maybe Player
getWinner board =
    let
        groups = 
            [ [0, 1, 2]
            , [3, 4, 5]
            , [6, 7, 8]
            , [0, 3, 6]
            , [1, 4, 7]
            , [2, 5, 8]
            , [0, 4, 8]
            , [2, 4, 6]
            ]
    in 
        groups
        |> List.filterMap (checkWin board)
        |> List.head


otherPlayer : Player -> Player
otherPlayer player =
    case player of
        Cross -> Zero
        Zero -> Cross


getBoard : Model -> Board
getBoard model =
    case model of
        Playing board ->
            board
        GameOver board _ ->
            board

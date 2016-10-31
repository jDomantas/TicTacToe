module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Util exposing (..)


stylesheet : Html Msg
stylesheet =
    let
        pureStylesheet =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href"      "http://yui.yahooapis.com/pure/0.6.0/pure-min.css"
            ]
        myStylesheet =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href"      "resources/styles.css"
            ]
    in 
        div []
            [ node "link" pureStylesheet []
            , node "link" myStylesheet []
            ]


displayCell : Cell -> Int -> Html Msg
displayCell cell index =
    let 
        styleClass = case cell of
            Empty -> "cell empty"
            Taken Cross -> "cell cross"
            Taken Zero -> "cell zero"
    in
        td
            [ onClick (CellClick index)
            , class styleClass
            ]
            []


displayBoard : Bool -> Board -> Html Msg
displayBoard active board =
    let
        getCell index = Maybe.withDefault Empty (getElement index board.cells)
        displayRow row =
            range (row * 3) (row * 3 + 2)
            |> List.map (\i -> displayCell (getCell i) i)
            |> \el -> tr [] el  
    in
        range 0 2
        |> List.map displayRow
        |> \rows -> table [] rows


playerName : Player -> String
playerName player =
    case player of
        Cross -> "orange"
        Zero -> "blue"


displayMenu : Maybe Player -> Html Msg
displayMenu winner =
    let
        winMessage =
            case winner of
                Just player ->
                    "Winner: " ++ playerName player
                Nothing ->
                    "Draw"
    in
        div [ class "menu" ] 
            [ p [] [ text winMessage ]
            , button 
                [ onClick Restart
                , class "pure-button pure-button-primary"
                ] [ text "New game" ]
            ]


displayGame : Model -> Html Msg
displayGame model = 
    case model of
        Playing board ->
            div [] 
                [ displayBoard True board
                ]
        GameOver board maybeWinner ->
            div []
                [ displayBoard False board
                , displayMenu maybeWinner
                ]


view : Model -> Html Msg
view model =
    div [] 
        [ stylesheet
        , displayGame model
        ]

module Game exposing (..)

import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, button, div, h1, hr, img, input, p, pre, text)
import Html.Attributes exposing (selected, src, value)
import Html.Events exposing (onClick, onInput)
import Monocle.Lens exposing (Lens)


type alias GameModel =
    { turn : Int
    , inventory : Inventory
    , machines : List Machine
    , log : List String
    }


type MachineOperation
    = Produce String Int
    | Convert ( String, Int ) ( String, Int )


type alias Machine =
    { id : Int
    , name : String
    , action : MachineOperation
    }


type alias Inventory =
    Dict String Int


type Msg
    = Tick
    | RemoveMachine Int


init : GameModel
init =
    { turn = 1
    , inventory = Dict.fromList [ ( "water", 10 ), ( "dirt", 5 ) ]
    , machines =
        [ Machine 0 "Dirt Digger" (Produce "dirt" 2)
        , Machine 1 "StoneDigger" (Produce "stone" 1)
        , Machine 2 "Well" (Produce "water" 1)
        , Machine 3 "StoneCrusher" (Convert ( "stone", 2 ) ( "dirt", 5 ))
        ]
    , log = [ "Game begins" ]
    }


inventoryLens =
    Lens .inventory (\i g -> { g | inventory = i })


update msg game =
    case msg of
        Tick ->
            tick game

        RemoveMachine id ->
            { game | machines = List.filter ((/=) id << .id) game.machines }


tick : GameModel -> GameModel
tick game =
    List.foldl
        (\f acc -> applyLog (f acc))
        game
        (List.map processMachine game.machines
            ++ [ addTurnAction ]
        )


applyLog : ( GameModel, String ) -> GameModel
applyLog ( game, logMessage ) =
    { game | log = logMessage :: game.log }


addTurnAction : GameModel -> ( GameModel, String )
addTurnAction game =
    ( { game | turn = game.turn + 1 }, "New Turn " ++ (String.fromInt <| game.turn + 1) )


addItem inventory item amount =
    let
        newAmount =
            amount + (Dict.get item inventory |> Maybe.withDefault 0)
    in
    Dict.insert item newAmount inventory


removeItem inventory item amount =
    let
        newAmount =
            (Dict.get item inventory |> Maybe.withDefault 0) - amount
    in
    if newAmount < 0 then
        Nothing

    else
        Just (Dict.insert item newAmount inventory)


processMachine : Machine -> GameModel -> ( GameModel, String )
processMachine { action, name } game =
    let
        inventory =
            inventoryLens.get game
    in
    case action of
        Produce item amount ->
            ( inventoryLens.set (addItem inventory item amount) game, name ++ " produced " ++ String.fromInt amount ++ " of " ++ item )

        Convert ( fromItem, fromAmount ) ( toItem, toAmount ) ->
            let
                remove =
                    removeItem inventory fromItem fromAmount
            in
            case remove of
                Nothing ->
                    ( game, name ++ " couldn't find " ++ String.fromInt fromAmount ++ " of " ++ fromItem )

                Just inventoryAfterRemove ->
                    ( inventoryLens.set (addItem inventoryAfterRemove toItem toAmount) game, name ++ " converted " ++ String.fromInt fromAmount ++ " of " ++ fromItem ++ " into " ++ String.fromInt toAmount ++ " of " ++ toItem )


viewInventory : Inventory -> Html msg
viewInventory inventory =
    div [] (Dict.toList inventory |> List.map viewItem)


viewItem : ( String, Int ) -> Html msg
viewItem ( name, amount ) =
    div [] [ text <| name ++ ": " ++ String.fromInt amount ]


viewMachines machines =
    div [] (List.map viewMachine machines)


viewMachine : Machine -> Html Msg
viewMachine { id, name, action } =
    div []
        [ text name
        , text
            (case action of
                Produce item amount ->
                    " produces " ++ String.fromInt amount ++ " of " ++ item ++ " per tick."

                Convert ( fromItem, fromAmount ) ( toItem, toAmount ) ->
                    " converts " ++ fromItem ++ " to " ++ toItem
            )
        , button [ onClick (RemoveMachine id) ] [ text "Remove it" ]
        ]


viewLog : List String -> Html msg
viewLog logs =
    div [] <| List.map (\s -> p [] [ text s ]) logs

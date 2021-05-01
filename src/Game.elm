module Game exposing (..)

import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, button, div, h1, hr, img, input, p, pre, text)
import Html.Attributes exposing (class, selected, src, value)
import Html.Events exposing (onClick, onInput)
import Inventory exposing (Inventory, Item)
import Monocle.Lens exposing (Lens)
import Random
import Style
import Util


type alias GameModel =
    { turn : Int
    , inventory : Inventory
    , machines : List Machine
    , log : List String
    , seed : Random.Seed
    }


type MachineOperation
    = Produce Item
    | Convert Item Item


type alias Machine =
    { id : String
    , name : String
    , action : MachineOperation
    }


type MachineType
    = DirtDigger
    | Well
    | StoneCrusher
    | StoneDigger


type Msg
    = Tick
    | RemoveMachine String


init : GameModel
init =
    { turn = 1
    , inventory = [ ( "water", 10 ), ( "dirt", 5 ) ] |> Util.dictFromTuples
    , machines = []
    , log = [ "Game begins" ]
    , seed = Random.initialSeed 555
    }
        |> (\model ->
                let
                    ( machines, seedLast ) =
                        List.foldr
                            (\( n, a ) ( l, s ) ->
                                let
                                    ( id, newSeed ) =
                                        Util.generateId s

                                    machine =
                                        mkMachine n a id
                                in
                                ( machine :: l, newSeed )
                            )
                            ( [], model.seed )
                            (List.map getMachineDefinition
                                [ DirtDigger
                                , StoneCrusher
                                , Well
                                , StoneDigger
                                ]
                            )
                in
                { model | machines = machines, seed = seedLast }
           )


getMachineDefinition : MachineType -> ( String, MachineOperation )
getMachineDefinition machineType =
    case machineType of
        DirtDigger ->
            ( "Dirt Digger", Produce ( "dirt", 2 ) )

        Well ->
            ( "Well", Produce ( "water", 1 ) )

        StoneCrusher ->
            ( "StoneCrusher", Convert ( "stone", 2 ) ( "dirt", 5 ) )

        StoneDigger ->
            ( "Stone Digger", Produce ( "stone", 1 ) )


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


mkMachine : String -> MachineOperation -> String -> Machine
mkMachine name action id =
    Machine id name action


applyLog : ( GameModel, String ) -> GameModel
applyLog ( game, logMessage ) =
    { game | log = logMessage :: game.log }


addTurnAction : GameModel -> ( GameModel, String )
addTurnAction game =
    ( { game | turn = game.turn + 1 }, "New Turn " ++ (String.fromInt <| game.turn + 1) )


processMachine : Machine -> GameModel -> ( GameModel, String )
processMachine { action, name } game =
    let
        inventory =
            inventoryLens.get game
    in
    case action of
        Produce item ->
            ( inventoryLens.set (Inventory.addItem inventory item) game, name ++ " produced " ++ Inventory.itemToString item )

        Convert fromItem toItem ->
            let
                remove =
                    Inventory.removeItem inventory fromItem
            in
            case remove of
                Nothing ->
                    ( game, name ++ " couldn't find " ++ Inventory.itemToString fromItem )

                Just inventoryAfterRemove ->
                    ( inventoryLens.set (Inventory.addItem inventoryAfterRemove toItem) game, name ++ " converted " ++ Inventory.itemToString fromItem ++ " into " ++ Inventory.itemToString toItem )


viewInventory : Inventory -> Html msg
viewInventory inventory =
    div [] (List.map viewItem <| Dict.values inventory)


viewItem : ( String, Int ) -> Html msg
viewItem ( name, amount ) =
    div [] [ text <| name ++ ": " ++ String.fromInt amount ]


viewMachines : List Machine -> Html Msg
viewMachines machines =
    div [] (List.map viewMachine machines)


viewMachine : Machine -> Html Msg
viewMachine { id, name, action } =
    div [ Style.machine ]
        [ text name
        , text id
        , text
            (case action of
                Produce item ->
                    " produces " ++ Inventory.itemToString item ++ " per tick."

                Convert fromItem toItem ->
                    " converts " ++ Inventory.itemToString fromItem ++ " to " ++ Inventory.itemToString toItem
            )
        , button
            [ onClick (RemoveMachine id)
            , Style.btnSmall
            , Style.btnBlue
            ]
            [ text "Remove it" ]
        ]


viewLog : List String -> Html msg
viewLog logs =
    div [] <| List.map (\s -> p [] [ text s ]) logs

module Game exposing (..)

import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, button, div, h1, hr, img, input, p, pre, text)
import Html.Attributes exposing (class, selected, src, value)
import Html.Events exposing (onClick, onInput)
import Inventory exposing (Inventory, Item)
import Items
import Monocle.Lens exposing (Lens)
import Random
import Style
import Util


type alias GameModel =
    { turn : Int
    , inventory : Inventory
    , buffer : Inventory
    , machines : List Machine
    , log : List String
    , seed : Random.Seed
    }


type MachineOperation
    = Input Item
    | Output Item


type alias Machine =
    { id : String
    , name : String
    , actions : List MachineOperation
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
    , inventory = [ ( Items.water, 10 ), ( Items.dirt, 5 ) ] |> Util.dictFromTuples
    , buffer = Dict.empty
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


getMachineDefinition : MachineType -> ( String, List MachineOperation )
getMachineDefinition machineType =
    case machineType of
        DirtDigger ->
            ( "Dirt Digger", [ Output ( Items.dirt, 2 ) ] )

        Well ->
            ( "Well", [ Output ( Items.water, 1 ) ] )

        StoneCrusher ->
            ( "StoneCrusher", [ Input ( Items.stone, 2 ), Output ( Items.dirt, 5 ) ] )

        StoneDigger ->
            ( "Stone Digger", [ Output ( Items.stone, 1 ) ] )


inventoryLens =
    Lens .inventory (\i g -> { g | inventory = i })


bufferLens =
    Lens .buffer (\b g -> { g | buffer = b })


update msg game =
    case msg of
        Tick ->
            tick game

        RemoveMachine id ->
            { game | machines = List.filter ((/=) id << .id) game.machines }


addBufferToInventory : GameModel -> GameModel
addBufferToInventory game =
    let
        bufferList =
            bufferLens.get game |> Dict.values

        inventory =
            inventoryLens.get game

        newInventory =
            List.foldl (\item inv -> Inventory.addItem inv item) inventory bufferList
    in
    inventoryLens.set newInventory game
        |> bufferLens.set Dict.empty


tick : GameModel -> GameModel
tick game =
    List.foldl
        (\f acc -> applyLog (f acc))
        game
        (List.map processMachine game.machines
            ++ [ addTurnAction ]
        )
        |> addBufferToInventory


mkMachine : String -> List MachineOperation -> String -> Machine
mkMachine name actions id =
    Machine id name actions


applyLog : ( GameModel, List String ) -> GameModel
applyLog ( game, logMessages ) =
    { game | log = logMessages ++ game.log }


addTurnAction : GameModel -> ( GameModel, List String )
addTurnAction game =
    ( { game | turn = game.turn + 1 }, [ "New Turn " ++ (String.fromInt <| game.turn + 1) ] )


processMachine : Machine -> GameModel -> ( GameModel, List String )
processMachine { actions, name } game =
    let
        --( inventory, buffer, logs ) =
        res =
            List.foldl
                (\action result ->
                    case result of
                        Ok ( oldInv, oldBuffer, oldlogs ) ->
                            case action of
                                Output item ->
                                    Ok ( oldInv, Inventory.addItem oldBuffer item, ("output" ++ Inventory.itemToString item) :: oldlogs )

                                Input item ->
                                    let
                                        remove =
                                            Inventory.removeItem oldInv item
                                    in
                                    case remove of
                                        Nothing ->
                                            Err ( oldInv, oldBuffer, ("I can't find " ++ Inventory.itemToString item) :: oldlogs )

                                        Just inventoryAfterRemove ->
                                            Ok ( inventoryAfterRemove, oldBuffer, ("I took " ++ Inventory.itemToString item) :: oldlogs )

                        Err err ->
                            Err err
                )
                (Ok ( inventoryLens.get game, bufferLens.get game, [] ))
                actions
    in
    case res of
        Ok ( inventory, buffer, logs ) ->
            ( game
                |> inventoryLens.set inventory
                |> bufferLens.set buffer
            , logs
            )

        Err ( _, _, logs ) ->
            ( game, logs )



--case action of
--    Produce item ->
--        ( inventoryLens.set (Inventory.addItem inventory item) game, name ++ " produced " ++ Inventory.itemToString item )
--
--    Convert fromItem toItem ->
--        let
--            remove =
--                Inventory.removeItem inventory fromItem
--        in
--        case remove of
--            Nothing ->
--                ( game, name ++ " couldn't find " ++ Inventory.itemToString fromItem )
--
--            Just inventoryAfterRemove ->
--                ( inventoryLens.set (Inventory.addItem inventoryAfterRemove toItem) game, name ++ " converted " ++ Inventory.itemToString fromItem ++ " into " ++ Inventory.itemToString toItem )


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
viewMachine { id, name, actions } =
    div [ Style.machine ]
        [ text name
        , text id

        --, text
        --    (case action of
        --        Produce item ->
        --            " produces " ++ Inventory.itemToString item ++ " per tick."
        --
        --        Convert fromItem toItem ->
        --            " converts " ++ Inventory.itemToString fromItem ++ " to " ++ Inventory.itemToString toItem
        --    )
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

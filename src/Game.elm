module Game exposing (..)

import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, button, div, h1, hr, img, input, p, pre, text)
import Html.Attributes exposing (selected, src, value)
import Html.Events exposing (onClick, onInput)
import Monocle.Lens exposing (Lens)
import OrderedDict


type alias GameModel =
    { turn : Int
    , inventory : Inventory
    , machines : List Machine
    , log : List String
    }


type MachineOperation
    = Produce Item
    | Convert Item Item


type alias Machine =
    { id : Int
    , name : String
    , action : MachineOperation
    }


type alias Item =
    ( String, Int )


type alias Inventory =
     OrderedDict.OrderedDict String Int


type Msg
    = Tick
    | RemoveMachine Int


init : GameModel
init =
    { turn = 1
    , inventory = [ ( "water", 10 ), ( "dirt", 5 ) ]
    , machines =
        [ Machine 0 "Dirt Digger" (Produce ( "dirt", 2 ))
        , Machine 1 "StoneDigger" (Produce ( "stone", 1 ))
        , Machine 2 "Well" (Produce ( "water", 1 ))
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


itemToString : Item -> String
itemToString ( name, amount ) =
    String.fromInt amount ++ " of " ++  name


applyLog : ( GameModel, String ) -> GameModel
applyLog ( game, logMessage ) =
    { game | log = logMessage :: game.log }


addTurnAction : GameModel -> ( GameModel, String )
addTurnAction game =
    ( { game | turn = game.turn + 1 }, "New Turn " ++ (String.fromInt <| game.turn + 1) )


addItem : Inventory -> Item -> Inventory
addItem inventory ( item, amount ) =
    let
        i =
            List.filter ((==) item << Tuple.first) inventory |> List.head |> Maybe.map Tuple.second

        newAmount =
            amount + (i |> Maybe.withDefault 0)
    in
    OrderedDict.insert item newAmount inventory


removeItem : Inventory-> (String, Int) -> Maybe Inventory
removeItem inventory ( item, amount ) =
    let
        newAmount =
            (OrderedDict.get item inventory |> Maybe.withDefault 0) - amount
    in
    if newAmount < 0 then
        Nothing

    else
        Just (OrderedDict.insert item newAmount inventory)


processMachine : Machine -> GameModel -> ( GameModel, String )
processMachine { action, name } game =
    let
        inventory =
            inventoryLens.get game
    in
    case action of
        Produce item ->
            ( inventoryLens.set (addItem inventory item) game, name ++ " produced " ++ itemToString item )

        Convert fromItem toItem ->
            let
                remove =
                    removeItem inventory fromItem
            in
            case remove of
                Nothing ->
                    ( game, name ++ " couldn't find " ++ itemToString fromItem )

                Just inventoryAfterRemove ->
                    ( inventoryLens.set (addItem inventoryAfterRemove toItem) game, name ++ " converted " ++ itemToString fromItem ++ " into " ++ itemToString toItem )


viewInventory : Inventory -> Html msg
viewInventory inventory =
    div [] (List.map viewItem inventory)


viewItem : ( String, Int ) -> Html msg
viewItem ( name, amount ) =
    div [] [ text <| name ++ ": " ++ String.fromInt amount ]


viewMachines : List Machine -> Html Msg
viewMachines machines =
    div [] (List.map viewMachine machines)


viewMachine : Machine -> Html Msg
viewMachine { id, name, action } =
    div []
        [ text name
        , text
            (case action of
                Produce item ->
                    " produces " ++ itemToString item ++ " per tick."

                Convert fromItem toItem ->
                    " converts " ++ itemToString fromItem ++ " to " ++ itemToString toItem
            )
        , button [ onClick (RemoveMachine id) ] [ text "Remove it" ]
        ]


viewLog : List String -> Html msg
viewLog logs =
    div [] <| List.map (\s -> p [] [ text s ]) logs

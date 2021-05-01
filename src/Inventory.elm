module Inventory exposing (..)

import Dict exposing (Dict)


type alias Item =
    ( String, Int )


type alias Inventory =
    Dict String Item


itemToString : Item -> String
itemToString ( name, amount ) =
    String.fromInt amount ++ " of " ++ name


addItem : Inventory -> Item -> Inventory
addItem inventory ( name, amount ) =
    let
        count =
            Dict.get name inventory |> Maybe.withDefault ( name, 0 ) |> Tuple.second

        newAmount =
            amount + count
    in
    Dict.insert name ( name, newAmount ) inventory


removeItem : Inventory -> ( String, Int ) -> Maybe Inventory
removeItem inventory ( name, amount ) =
    let
        newAmount =
            (Dict.get name inventory |> Maybe.withDefault ( name, 0 ) |> Tuple.second) - amount
    in
    if newAmount < 0 then
        Nothing

    else
        Just (Dict.insert name ( name, newAmount ) inventory)

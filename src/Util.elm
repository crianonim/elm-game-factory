module Util exposing (..)

import Array
import Dict exposing (Dict)
import Random


dictFromTuples : List ( comparable, a ) -> Dict comparable ( comparable, a )
dictFromTuples =
    dict Tuple.first


dict : (a -> comparable) -> List a -> Dict comparable a
dict fn x =
    x
        |> List.map (\y -> ( fn y, y ))
        |> Dict.fromList


generateId : Random.Seed -> ( String, Random.Seed )
generateId seed =
    let
        chars =
            Array.fromList <| String.split "" "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

        idLength =
            12

        cLength =
            Array.length chars
    in
    List.foldl
        (\_ ( l, s ) ->
            let
                ( val, newSeed ) =
                    Random.step (Random.int 0 cLength) s
            in
            ( (Array.get val chars |> Maybe.withDefault "X") ++ l, newSeed )
        )
        ( "", seed )
        (List.range 0 idLength)

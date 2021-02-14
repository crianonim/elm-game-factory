module OrderedDict exposing (..)

type alias OrderedDict k v =
    List ( k, v )


get : k -> OrderedDict k v -> Maybe v
get k dict =
    List.filter ((==) k << Tuple.first) dict
        |> List.head
        |> Maybe.map Tuple.second


insert : k -> v -> OrderedDict k v -> OrderedDict k v
insert k v dict =
    let
        existing =
            List.filter ((==) k << Tuple.first) dict
                |> List.head
    in
    case existing of
        Nothing ->
            dict ++ [ ( k, v ) ]

        Just ( _, _ ) ->
            List.map
                (\( kk, vv ) ->
                    if kk == k then
                        ( k, v )

                    else
                        ( kk, vv )
                )
                dict

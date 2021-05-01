module Util exposing (..)

import Dict exposing (Dict)
dictFromTuples : List (comparable, a) -> Dict comparable (comparable,a)
dictFromTuples =
    dict Tuple.first

dict : (a->comparable) -> List a -> Dict comparable a
dict fn x =
    x
     |> List.map (\y-> (fn y,y) )
     |> Dict.fromList
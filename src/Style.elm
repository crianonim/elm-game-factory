module Style exposing (..)

import Html exposing  (..)
import Html.Attributes exposing (..)


border_2 : Attribute msg
border_2 = class "border-2"
btn = class "border-2 border-transparent bg-red-500 ml-3 py-2 px-4 font-bold uppercase text-white rounded transition-all hover:border-red-500 hover:bg-transparent hover:text-red-500"

btnSmall = class "border-2 border-transparent ml-3 py-1 px-2 font-bold uppercase text-white rounded transition-all hover:bg-transparent"
btnRed = class "hover:text-red-500 bg-red-500"
btnBlue = class "hover:text-blue-500 bg-blue-500 hover:border-blue-500"
bgWhite = class "bg-white"
bgRed = class "bg-red-500"
textWhite = class "text-white"


machine = class "rounded-xl p-3 w-60 h-60 border-2"
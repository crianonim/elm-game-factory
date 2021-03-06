module Main exposing (main)

import Browser
import Game
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Platform.Cmd exposing (Cmd)
import Random
import Util


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { seed = Random.initialSeed 666
      , roll = Nothing
      , game = Game.init
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateRoll ->
            let
                ( result, seed ) =
                    Random.step (Random.int 1 6) model.seed
            in
            ( { model | seed = seed, roll = Just result }, Cmd.none )

        Tick ->
            ( { model | game = Game.tick model.game }, Cmd.none )

        GameMsg m ->
            ( { model | game = Game.update m model.game }, Cmd.none )


type alias Model =
    { seed : Random.Seed
    , roll : Maybe Int
    , game : Game.GameModel
    }


type Msg
    = GenerateRoll
    | Tick
    | GameMsg Game.Msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ button [ onClick GenerateRoll ] [ text "Roll" ]
            , text ("Roll: " ++ (Maybe.map String.fromInt model.roll |> Maybe.withDefault "No rolled"))
            ]
        , div []
            [ text <| "Game Turn " ++ String.fromInt model.game.turn
            , button [ onClick Tick ] [ text "Tick" ]
            , h6 [] [ text "Inventory" ]
            , Game.viewInventory model.game.inventory
            , h6 [] [ text "Buffer" ]
            , Game.viewInventory model.game.buffer
            , Game.viewMachines model.game.machines |> Html.map GameMsg
            ]
        , div [] [ Game.viewLog model.game.log ]
        ]

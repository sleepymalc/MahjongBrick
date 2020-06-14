module Breakout exposing (main)

import Html exposing (..)
import Browser

import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp, onResize)
import Html.Events exposing (keyCode)
import Json.Decode as Decode
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Model exposing (Model,State(..),init)
import View exposing (view)
import Message exposing (Msg(..),MoveDirection(..),PlayerNum(..))
import Update exposing (update)

main =
    Browser.element
        { init = init
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }




subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ --if model.state == Playing then
            onAnimationFrameDelta Tick
          --else
            --Sub.none
        , onKeyUp (Decode.map (key False) keyCode)
        , onKeyDown (Decode.map (key True) keyCode)
        , onResize Resize
        ]

key : Bool -> Int -> Msg
key on keycode =
    case keycode of
    --Player1
        65 ->
            Move Player1 Left on

        68 ->
            Move Player1 Right on

        87 ->
            Move Player1 Up on

        83 ->
            Move Player1 Down on

    --Player2
        37 ->
            Move Player2 Left on

        39 ->
            Move Player2 Right on

        38 ->
            Move Player2 Up on

        40 ->
            Move Player2 Down on


        _ ->
            Noop
        
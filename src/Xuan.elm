module Xuan exposing (main)

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
        , onKeyUp (Decode.map (key model.state False) keyCode)
        , onKeyDown (Decode.map (key model.state True) keyCode)
        , onResize Resize
        ]



--key : Bool -> Int -> Msg
key state on keycode =
    case state of
        Playing ->
            playingKey on keycode
        Model.Story _ ->
            if on then
                storyKey keycode
            else
                Noop
        Model.Rule ->
            if on then
                storyKey keycode
            else
                Noop
        _ -> Noop


storyKey keycode=
    case keycode of
        37 ->
            Turn -1 
        39 ->
            Turn 1 
        _-> Noop

playingKey on keycode=
    case keycode of
    --Player1
        65 ->
            Move Player1 Left on     

        68 ->
            Move Player1 Right on

        87 ->
            MoveHandcard Player1 Left on

        83 ->
            MoveHandcard Player1 Right on

    --Player2
        37 ->
            Move Player2 Left on

        39 ->
            Move Player2 Right on

        38 ->
            MoveHandcard Player2 Left on

        40 ->
            MoveHandcard Player2 Right on


        _ ->
            Noop

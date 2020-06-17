module Update exposing (update)

import Message exposing (Msg(..),MoveDirection(..),PlayerNum(..))
import Model exposing (..)
import Random
import Animate exposing (animate)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GetViewport { viewport } ->
            ( { model
                | size = Vector viewport.width viewport.height
              }
            , Cmd.none
            )
            
        Resize width height -> 
            ( { model 
                | size = Vector (toFloat width) (toFloat height )
                }
            , Cmd.none
            )

        Start ->
            ( { model
                | state = Playing
                , player1 = model.player1 |> setBallSpeed Model.attribute.defaultBallSpeed 
                , player2 = model.player2 |> setBallSpeed Model.attribute.defaultBallSpeed
              }
            , Random.generate NewBricks Model.randomList
            )

        NewBricks values->
            ( initBricks values model
            , Cmd.none
            )

        Move player moveDirection on->
            case player of
                Player1 ->
                    ({model|player1=model.player1 |> setPaddleSpeed ( getSpeedDirection moveDirection on *8 ) },Cmd.none)
                Player2 ->
                    ({model|player2=model.player2 |> setPaddleSpeed ( getSpeedDirection moveDirection on *8 ) },Cmd.none)

        MoveHandcard player moveDirection on->
            case player of
                Player1 ->
                    ({model|player1=model.player1 |> moveChosencard (getSpeedDirection moveDirection on) },Cmd.none)
                Player2 ->
                    ({model|player2=model.player2 |> moveChosencard (getSpeedDirection moveDirection on) },Cmd.none)

        Tick time ->
            case model.state of
                Win _ ->
                    (model , Cmd.none )
                Playing ->
                    (model |> animate time , Cmd.none )
                Paused ->
                    (model |> animate time , Cmd.none )

        Noop ->
            ( model, Cmd.none )



getSpeedDirection moveDirection on =
    if on then
        case moveDirection of
            Left ->
                -1
            Right ->
                1  
    else
        0

moveChosencard: Int -> Player -> Player
moveChosencard x player =
    {player|chosenCard = player.chosenCard+x}

setPaddleSpeed: Float -> Player -> Player--helper
setPaddleSpeed speed player =
    let
        paddle = player.paddle
        newPaddle = {paddle| speed=speed}
    in
        {player|paddle=newPaddle}

setBallSpeed: Vector Float -> Player-> Player--helper
setBallSpeed speed player =
    let
        ball = player.ball
        newBall = {ball| speed=speed}
    in
        {player|ball=newBall}


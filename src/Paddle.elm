module Paddle exposing (movePaddle,partPaddle)

import Model exposing (..)
import Model exposing (PlayingState(..))



movePaddle: Float->Player->Player
movePaddle time player=
    let
        paddle = player.paddle

        speed = paddle
                |> nextSpeed time
                |> restrictedSpeed paddle
                |> blockedSpeed player.state paddle
        pos = paddle 
                |> nextPos 
                |> loopPos
        newPaddle = 
            { paddle
            | pos= pos
            , speed = speed
            }
    in
        {player|paddle = newPaddle}


partPaddle paddle= 
    let
        sizeX = ((nextPos paddle).x+paddle.size.x) - Model.attribute.range.x
    in
        {paddle| pos = Vector 0 paddle.pos.y, size = Vector sizeX paddle.size.y}

nextPos a= 
    addCoeffientVector 1 a.pos a.speed

addCoeffientVector: Float -> Vector Float -> Vector Float -> Vector Float
addCoeffientVector a x y=
    Vector (a*x.x+y.x) (a*x.y+y.y)


loopPos pos= 
    let
        x = if pos.x<0 then
                pos.x+ attribute.range.x 
            else if pos.x > attribute.range.x then
                pos.x- attribute.range.x 
            else 
                pos.x
        y = pos.y
    in 
        Vector x y


nextSpeed time paddle= paddle.speed
    |>addCoeffientVector (time/100) paddle.accelaration

restrictedSpeed paddle speed = 
    if abs speed.x > Model.attribute.maxPaddleSpeedX then
        paddle.speed
    else 
        speed

blockedSpeed state paddle speed=
    case state of
        Winter _->
            speed
        _ -> 
            if paddle.accelaration.x == 0 then 
                Vector 0 0
            else
                speed

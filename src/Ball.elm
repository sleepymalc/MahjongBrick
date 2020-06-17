module Ball exposing (moveBall,collideWith)
import Paddle exposing (partPaddle)

import Model exposing (..)
--move player's ball with respect to bricks, state, paddle, wall
moveBall: State -> List Brick-> Player-> Player--animate helper
moveBall state bricks player=
    let 
        ball= player.ball
            |>punish
            |>collideWallY
            |>collideWallX
            |>collideBricks bricks
            |>stayWithPaddle state player.paddle
            |>collidePaddle player.paddle
            
        newBall = {ball|pos= addVector ball.pos ball.speed,imgIndex=modBy 6 (ball.imgIndex+1)}
    in
    {player|ball=newBall}


punish ball=
    if (ball.punish == False) then
        {ball|speed=ball.speed}
    else if ((ball.pos.y+ball.speed.y)>(Model.attribute.range.y*2/3)) then
        {ball | punish=False}
    else
        {ball|speed= addVector ball.speed (Vector 0 0.01) }


collideWallY ball= 
    if (ball.pos.y + ball.speed.y) |>inRange 0 ((Model.attribute.range.y-ball.size.y)*0.8)  then
       { ball|speed = ball.speed}
    else if ( (ball.pos.y + ball.speed.y) > ( ((Model.attribute.range.y)*0.5) )) then
        {ball| pos= Vector (Model.attribute.range.x/2-30/2) (Model.attribute.range.y*1/2) , speed=Vector 0 0,punish=True}
    else
        {ball|speed = changeDirection Y ball.speed }


collideWallX ball= 
    if (ball.pos.x + ball.speed.x)|>inRange 0 (Model.attribute.range.x-ball.size.x)   then
        {ball|speed = ball.speed}
    else 
        {ball|speed = changeDirection X ball.speed }


collideBricks bricks ball=
    let
        x=if List.any (\brick->(
                (Tuple.first(collideWith ball brick))
                && (((ball.pos.x)|> inRange (brick.pos.x-ball.size.x) (brick.pos.x+brick.size.x))==False)
                ))
             
            bricks  
            then
                -ball.speed.x 
            else 
                ball.speed.x

        y=if List.any (\brick->(
                ((ball.pos.x+ball.speed.x)|> inRange (brick.pos.x-ball.size.x) (brick.pos.x+brick.size.x)) 
                &&((ball.pos.y+ball.speed.y)|>inRange (brick.pos.y-ball.size.y) (brick.pos.y+brick.size.y)) 
                && ((ball.pos.y |>inRange (brick.pos.y-ball.size.y) (brick.pos.y+brick.size.y)) ==False)))
           bricks
            then
                -ball.speed.y
            else 
                ball.speed.y
    in
        {ball|speed = {x=x,y=y}  }

--Judge whether the brick collide with the ball
--Might update: not judged by virtually real size of the brick 
--collideWith: Ball->Brick->Bool--animate helper
collideWith ball brick =
    let
        boolX=(ball.pos.x+ball.speed.x)|> inRange (brick.pos.x-ball.size.x) (brick.pos.x+brick.size.x)
        boolY=(ball.pos.y+ball.speed.y)|> inRange (brick.pos.y-ball.size.y) (brick.pos.y+brick.size.y)
    in
    if (boolX&&boolY) then
        if (ball.pos.y)|> inRange (brick.pos.y) (brick.pos.y+brick.size.y) then
            (True,{brick|count=brick.count-1})
        else
            (True,{brick|count=brick.count-2})
    else 
        (False,brick)



stayWithPaddle state paddle ball =
    case state of
        Playing ->
            {ball|speed = ball.speed}
        Paused ->
            {ball|speed = paddle.speed}
        Win _ ->
            {ball|speed = ball.speed}


judgeCollidePaddleBall paddle ball=
    ((ball.pos.x + ball.speed.x) 
        |> inRange (paddle.pos.x - ball.size.x) (paddle.pos.x+paddle.size.x)) --might update
    && ((ball.pos.y + ball.speed.y + ball.size.y)>(paddle.pos.y)  )
    && ((ball.pos.y+ball.size.y)<(paddle.pos.y))

collidePaddle paddle ball =
    if judgeCollidePaddleBall paddle ball
    || judgeCollidePaddleBall (partPaddle paddle) ball
    then 
         changeBallSpeed paddle ball
    else 
         {ball|speed = ball.speed}

changeBallSpeed paddle ball=
        {ball|speed = Vector (ball.speed.x+(0.2*paddle.speed.x)) (-ball.speed.y)}



inRange minx maxx x=
    minx<=x && x<=maxx


addVector x y =
    Vector (x.x+y.x) (x.y+y.y) 

type Direction = X | Y
    

changeDirection direction v =
    case direction of
        Y ->
            { x = v.x, y = -v.y }
        X ->
            { x = -v.x, y = v.y }

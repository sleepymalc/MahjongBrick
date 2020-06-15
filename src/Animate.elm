module Animate exposing (animate)

import Model exposing (..)
import Message exposing (Msg(..),MoveDirection(..),PlayerNum(..))
import Mahjong exposing (formHu)


animate: Float -> Model ->Model -- Might update: Haven't use time
animate time model =
    let 
        (vaildBricks,invaildBricks) = model.bricks
            |> List.partition (\brick-> brick.pos.y>=0)
            
        aftercollide_1=(List.map (\brick-> (collideWith model.player1.ball brick)) vaildBricks)

        aftercollidebricks_1=Tuple.second (List.unzip aftercollide_1)

        aftercollide_2=(List.map (\brick-> (collideWith model.player2.ball brick)) aftercollidebricks_1)
        aftercollidebricks_2=Tuple.second (List.unzip aftercollide_2)

        (eliminated_1, rest)= 
            List.partition 
                (\brick-> Tuple.first(brick |> (collideWith model.player1.ball)) && brick.count<=0) 
                aftercollidebricks_2
        (eliminated_2, newrest)=
            List.partition
                (\brick-> Tuple.first(brick |> (collideWith model.player2.ball)) && brick.count<=0)
                rest

        audioList = 
            if List.isEmpty (eliminated_1 ++ eliminated_2)==False then
                "bgm/dice.wav"::model.audioList
            else 
                model.audioList

        player1 = model.player1
                |> updateChosenCard
                |> catchHandcard
                |> movePaddle
                |> moveBall model.state vaildBricks
                |> moveFallingcard
                |> addFallingcard eliminated_1

        player2 = model.player2
                |> updateChosenCard
                |> catchHandcard
                |> movePaddle
                |> moveBall model.state vaildBricks
                |> moveFallingcard
                |> addFallingcard eliminated_2
                
        bricks = moveBricks (newrest ++ invaildBricks)

        state = updateState player1 player2 model.state

    in
        { model
            | player1=player1
            , player2=player2
            , bricks=bricks
            , audioList=audioList
            , state=state
        }


-- updateState
updateState player1 player2 state= 
    if win player1 then
        Win Player1
    else if win player2 then
        Win Player2
    else
        state

win player =
    case List.head player.droppedcard of
        Nothing ->
            False
        Just card ->
            card
            |> getHandcard player.handcard
            |> formHu

getHandcard handcard card= 
    (card::handcard)
    |> List.sortBy .suit


-- updateChosenCard
updateChosenCard player =
    { player
    | chosenCard = modBy Model.attribute.handcardNum (player.chosenCard + player.moveHandcard)}


-- moveBricks
moveBricks bricks =
    if List.any (\brick-> brick.pos.y >= (Model.attribute.bricksNum.y-1)*Model.brickHeight) bricks then
        bricks
    else
        List.map (\brick-> {brick| pos = Vector brick.pos.x (brick.pos.y+1)}) bricks

-- addFallingcard
addFallingcard eliminated player=
    {player | fallingcard = player.fallingcard ++ eliminated}


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



collideWithPaddle paddle brick =
    (((paddle.pos.y)|>inRange (brick.pos.y+brick.size.y) (brick.pos.y+brick.size.y+paddle.size.y+1))
  &&((paddle.pos.x+paddle.speed+paddle.size.x) > (brick.pos.x))
  &&((paddle.pos.x+paddle.speed)< (brick.pos.x+brick.size.x)))


-- catchHandcard
catchHandcard: Player -> Player
catchHandcard player =
    let
        (handcards,fallingcard)=
            List.partition
                    (\brick-> (brick |> collideWithPaddle player.paddle))
                    player.fallingcard 
        newPlayer = List.foldl dropCard player handcards


        newHandcard = newPlayer.handcard
            |>List.sortBy .suit 
            |>List.map2 
                (\posx card->
                    {card | 
                        pos=Vector (posx*handcardSizeRate+ handcardSetOff newPlayer.handcard) 0}
                    ) (Model.posXList 13) 
            |>List.indexedMap 
                (\index card->
                    if index == player.chosenCard then
                        {card|pos=Vector card.pos.x (Model.attribute.handcardPosY-10) }
                    else
                        {card|pos=Vector card.pos.x Model.attribute.handcardPosY })
        newFallingcard=List.filter (\card->card.pos.y<(Model.attribute.range.y*2/3+25)) fallingcard
    in
        { newPlayer
        | fallingcard = newFallingcard
        , handcard = newHandcard}

handcardSetOff handcard=
    (Model.attribute.range.x-(toFloat(List.length handcard))*Model.brickWidth*handcardSizeRate)/2

handcardSizeRate =
    (toFloat Model.attribute.bricksNum.x)/(toFloat Model.attribute.handcardNum)


swapSuit: Brick -> Brick -> (Brick,Brick)
swapSuit card1 card2=
    ( {card1|suit = card2.suit}
    , {card2|suit = card1.suit}
    )

chooseCard: Player -> Maybe Brick
chooseCard player= player.handcard
    |> List.drop (player.chosenCard) 
    |> List.head


droppingCard: Player -> Brick -> Brick
droppingCard player handcard= case (chooseCard player) of
            Nothing ->
                handcard
            Just card ->
                card

dropCard: Brick -> Player -> Player
dropCard handcard player  =
    let
        ( added,dropped ) = swapSuit (droppingCard player handcard) handcard
        droppedcard = dropped :: player.droppedcard

        newHandcard = player.handcard
            |>List.map 
                (\card->
                    if card.pos.x == added.pos.x then
                        { card|suit = added.suit}
                    else card
                )
    in
        { player
        | droppedcard = droppedcard
        , handcard = newHandcard
        }


     

--move player's paddle with respect to the positino range and paddle's speed
--movePaddle: Player->Player--animate helper
movePaddle player=
    let
        paddle = player.paddle
        newPaddle = {paddle|pos= Vector (paddle.pos.x+(paddleSpeed paddle)) paddle.pos.y}
    in
        {player|paddle = newPaddle}
--paddleSpeed: Paddle->Float--movePaddle helper
paddleSpeed paddle =
    if (paddle.pos.x + paddle.speed)|>inRange 0 (Model.attribute.range.x-paddle.size.x) then
        paddle.speed
    else 0



--move player's falling cards
moveFallingcard player=
    let
        fallingcards = List.map (\card-> {card|pos={x=card.pos.x,y=card.pos.y+1}}) player.fallingcard
    in
        {player|fallingcard=fallingcards}



--move player's ball with respect to bricks, state, paddle, wall
--moveBall: State -> List Brick-> Player--animate helper
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


stayWithPaddle state paddle ball =
    case state of
        Playing ->
            {ball|speed = ball.speed}
        Paused ->
            {ball|speed = Vector paddle.speed 0}
        Win _ ->
            {ball|speed = ball.speed}


collidePaddle paddle ball =
    if ((ball.pos.x + ball.speed.x) 
        |> inRange (paddle.pos.x - ball.size.x) (paddle.pos.x+paddle.size.x)) --might update
    && ((ball.pos.y + ball.speed.y + ball.size.y)>(paddle.pos.y)  )
    && ((ball.pos.y+ball.size.y)<(paddle.pos.y)  )
    then 
         changeBallSpeed paddle ball
    else 
         {ball|speed = ball.speed}

changeBallSpeed paddle ball=
    {ball|speed = { x=(changeDirection Y ball.speed).x+(0.5*paddle.speed) ,y=(changeDirection Y ball.speed).y}}



inRange minx maxx x=
    minx<=x && x<=maxx


addVector a b =
    {x=a.x+b.x,y=a.y+b.y}



type Direction = X | Y
    

changeDirection direction v =
    case direction of
        Y ->
            { x = v.x, y = -v.y }
        X ->
            { x = -v.x, y = v.y }
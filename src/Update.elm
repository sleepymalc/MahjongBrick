module Update exposing (update,addVector)

import Message exposing (Msg(..),MoveDirection(..),PlayerNum(..))
import Model exposing (..)
import Random
import List exposing (member)
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
            ( (initBricks values model)
            , Cmd.none
            )

        Move player moveDirection on->
            let
                moveX=case moveDirection of
                            Left ->
                                -1
                            Right ->
                                1    

                speed = if on then
                            moveX*8
                        else
                            0
 
            in 
                case player of
                    Player1 ->
                        ({model|player1=model.player1 |> setPaddleSpeed speed },Cmd.none)
                    Player2 ->
                        ({model|player2=model.player2 |> setPaddleSpeed speed },Cmd.none)

        MoveHandcard player moveDirection on->
            let
                moveX=case moveDirection of
                            Left ->
                                -1
                            Right ->
                                1    
                speed = if on then
                            moveX
                        else
                            0

            in 
                case player of
                    Player1 ->
                        ({model|player1=model.player1 |> moveChosencard speed },Cmd.none)
                    Player2 ->
                        ({model|player2=model.player2 |> moveChosencard speed },Cmd.none)


        Tick time ->
                case model.state of
                    Win _ ->
                        (model , Cmd.none )
                    Playing ->
                        (model
                            |> animate time , Cmd.none )
                    Paused ->
                        (model
                            |> animate time , Cmd.none )

        Noop ->
            ( model, Cmd.none )

    
animate: Float -> Model ->Model
-- Might update: Haven't use time
animate time model =
    let 

        (eliminated_1, rest)= 
            List.partition 
                (\brick-> (brick |> collideWith model.player1.ball)) 
                model.bricks
        (eliminated_2, newrest)=
            List.partition
                (\brick-> (brick |> collideWith model.player2.ball))
                rest

        audioList = 
            if List.isEmpty (List.append eliminated_2 eliminated_1)==False then
                "bgm/dice.wav"::model.audioList
            else 
                model.audioList

        vaildBricks = model.bricks
            |> List.filter (\brick-> brick.pos.y>=0)

        player1 = model.player1
                |> moveHandcard
                |> catchHandcard
                |> movePaddle
                |> moveBall model.state vaildBricks
                |> moveFallingcard
                |> addFallingcard eliminated_1

        player2 = model.player2
                |> moveHandcard
                |> catchHandcard
                |> movePaddle
                |> moveBall model.state vaildBricks
                |> moveFallingcard
                |> addFallingcard eliminated_2
                
        bricks = moveBricks newrest

        state = if win player1 then
                Win Player1
            else if win player2 then
                Win Player2
            else
                model.state

    in
        
        { model
            | player1=player1
            , player2=player2
            , bricks=bricks
            , audioList=audioList
            , state=state
        }

win player =
    case List.head player.droppedcard of
        Nothing ->
            False
        Just card ->
            let
                handcard = (card::player.handcard)
                    |> List.sortBy .suit
            in
                formHu handcard

moveHandcard player =
    { player
    | chosenCard = modBy Model.attribute.handcardNum (player.chosenCard + player.moveHandcard)}

formPongs bricks = 
    let
        pongs = bricks
            |> List.take 3
        brick1 = pongs
            |> List.take 1
        brick2 = pongs
            |> List.drop 1
            |> List.take 1

        brick3 = pongs
            |> List.drop 2
            |> List.head 
    in
        case brick3 of
            Nothing -> 
                False
            Just brick ->
                (List.member brick.suit (List.map .suit brick1)) && (List.member brick.suit (List.map .suit brick1))

vaildKong suit =
    ( suit <=27 ) && ( modBy 9 suit /=0 ) && ( modBy 9 suit /=8 )

formKongHelper brick bricks =
    if vaildKong brick.suit then
        (List.member (brick.suit+1) (List.map .suit bricks))
        && (List.member (brick.suit+2) (List.map .suit bricks))
    else 
        False

formKong bricks = 
    case (List.head bricks) of
        Nothing -> 
            False
        Just brick ->
            formKongHelper brick bricks

formChow bricks = 
    let
        chow = bricks
            |> List.take 2 
        brick1 = chow
            |> List.take 1
        brick2 = chow
            |> List.drop 1
            |> List.head 
        
    in
        case brick2 of
            Nothing -> 
                False
            Just brick ->
                List.member brick.suit (List.map .suit brick1)

dropBrick suit bricks =
    let
        ( sameBricks,rest ) = List.partition (\brick -> brick.suit == suit) bricks
    in
        (List.append rest (List.drop 1 sameBricks))
        


dropKong bricks =
    let
        suit1 = case (List.head bricks) of
            Nothing -> 
                0
            Just brick ->
                brick.suit
    in
        bricks
        |> dropBrick suit1
        |> dropBrick (suit1 + 1)
        |> dropBrick (suit1 + 2)
        |> List.sortBy .suit

--formChow bricks = 

formHu bricks =
    if List.length bricks == 0 then
        True
    else
        ( (formPongs bricks) && (formHu (List.drop 3 bricks)) )
        || ( (formKong bricks) && (formHu (dropKong bricks)) )
        || ( (modBy 3 (List.length bricks) == 2)&&(formChow bricks)&&(formHu (List.drop 2 bricks)) )


moveBricks bricks =
    if List.any (\brick-> brick.pos.y >= (Model.attribute.bricksNum.y-1)*Model.brickHeight) bricks then
        bricks
    else
        List.map (\brick-> {brick| pos = Vector brick.pos.x (brick.pos.y+1)}) bricks

addFallingcard eliminated player=
    {player | fallingcard = List.append player.fallingcard eliminated}


--Judge whether the brick collide with the ball
--Might update: not judged by virtually real size of the brick 
--collideWith: Ball->Brick->Bool--animate helper
collideWith ball brick =
    ((ball.pos.x+ball.speed.x)|> inRange (brick.pos.x-ball.size.x) (brick.pos.x+brick.size.x)) 
     &&((ball.pos.y+ball.speed.y)|>inRange (brick.pos.y-ball.size.y) (brick.pos.y+brick.size.y))


collideWithPaddle paddle brick =
    (((paddle.pos.y)|>inRange (brick.pos.y+brick.size.y) (brick.pos.y+brick.size.y+paddle.size.y+1))
  &&((paddle.pos.x+paddle.speed+paddle.size.x) > (brick.pos.x))
  &&((paddle.pos.x+paddle.speed)< (brick.pos.x+brick.size.x)))

handcardSetOff handcard=
    (Model.attribute.range.x-(toFloat(List.length handcard))*Model.brickWidth)/2


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
<<<<<<< HEAD
                        pos=Vector (posx+ handcardSetOff(List.append handcard player.handcard)) Model.attribute.handcardPosY}
                    ) (Model.posXList 13)
=======
                        pos=Vector (posx*handcardSizeRate+ handcardSetOff newPlayer.handcard) 0}
                    ) (Model.posXList 13) 
            |>List.indexedMap 
                (\index card->
                    if index == player.chosenCard then
                        {card|pos=Vector card.pos.x (Model.attribute.handcardPosY-10) }
                    else
                        {card|pos=Vector card.pos.x Model.attribute.handcardPosY })
>>>>>>> 1033a35e099c39746c00bc2dde9620524a299e43
        newFallingcard=List.filter (\card->card.pos.y<(Model.attribute.range.y*2/3+25)) fallingcard
    in
        { newPlayer
        | fallingcard = newFallingcard
        , handcard = newHandcard}
   -- ((bricks.pos.y+bricks.size.y)>paddle.pos.y&&(bricks.pos.x |>inRange paddle.pos.x paddle.pos.x-))
     

--move player's paddle with respect to the positino range and paddle's speed
--movePaddle: Player->Player--animate helper
movePaddle player=
    let
        paddle = player.paddle
        newPaddle = {paddle|pos= Vector (paddle.pos.x+(paddleSpeed paddle)) paddle.pos.y}
    in
        {player|paddle = newPaddle}
--move player's falling cards
moveFallingcard player=
    let
        fallingcards = List.map (\card-> {card|pos={x=card.pos.x,y=card.pos.y+1}}) player.fallingcard
    in
        {player|fallingcard=fallingcards}


--paddleSpeed: Paddle->Float--movePaddle helper
paddleSpeed paddle =
    if (paddle.pos.x + paddle.speed)|>inRange 0 (Model.attribute.range.x-paddle.size.x) then
        paddle.speed
    else 0


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


collideBricks bricks ball=
    let
        x=if List.any (\brick->(
                (collideWith ball brick)
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

collideWallX ball= 
    if (ball.pos.x + ball.speed.x)|>inRange 0 (Model.attribute.range.x-ball.size.x)   then
        {ball|speed = ball.speed}
    else 
        {ball|speed = changeDirection X ball.speed }

collideWallY ball= 
    if (ball.pos.y + ball.speed.y) |>inRange 0 ((Model.attribute.range.y-ball.size.y)*0.8)  then
       { ball|speed = ball.speed}
    else if ( (ball.pos.y + ball.speed.y) > ( ((Model.attribute.range.y)*0.5) )) then
        {ball| pos= Vector (Model.attribute.range.x/2-30/2) (Model.attribute.range.y*1/2) , speed=Vector 0 0,punish=True}
    else
        {ball|speed = changeDirection Y ball.speed }

punish ball=
    if (ball.punish == False) then
        {ball|speed=ball.speed}
    else if ((ball.pos.y+ball.speed.y)>(Model.attribute.range.y*2/3)) then
        {ball | punish=False}
    else
        {ball|speed= addVector ball.speed (Vector 0 0.01) }


changeBallSpeed paddle ball=
    {ball|speed = { x=(changeDirection Y ball.speed).x+(0.5*paddle.speed) ,y=(changeDirection Y ball.speed).y}}

collidePaddle paddle ball =
    if ((ball.pos.x + ball.speed.x) 
        |> inRange (paddle.pos.x - ball.size.x) (paddle.pos.x+paddle.size.x)) --might update
    && ((ball.pos.y + ball.speed.y + ball.size.y)>(paddle.pos.y)  )
    && ((ball.pos.y+ball.size.y)<(paddle.pos.y)  )
    then 
         changeBallSpeed paddle ball
    else 
         {ball|speed = ball.speed}

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

--setPaddleSpeed: Float -> Player -> Player--helper
setPaddleSpeed speed player =
    let
        paddle = player.paddle
        newPaddle = {paddle| speed=speed}
    in
        {player|paddle=newPaddle}

--setBallSpeed: Vector->Player->Player--helper
setBallSpeed speed player =
    let
        ball = player.ball
        newBall = {ball| speed=speed}
    in
        {player|ball=newBall}

moveChosencard x player =
    {player|chosenCard = player.chosenCard+x}
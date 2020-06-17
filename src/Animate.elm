module Animate exposing (animate)
import Paddle exposing (movePaddle)
import Ball exposing (moveBall,collideWith)
import Card exposing (catchHandcard,moveBricks)
import Skill exposing (tickState,applySkill)

import Model exposing (..)
import Message exposing (Msg(..),MoveDirection(..),PlayerNum(..))
import Mahjong exposing (formHu)
import Model exposing (PlayingState(..))


animate: Float -> Model ->Model
animate time model =
    let 
        

        (vaildBricks,invaildBricks) = model.bricks
            |> List.partition (\brick-> brick.pos.y+brick.size.y >0)
            
        aftercollidebricks_1=vaildBricks
            |>List.map (\brick-> collideWith model.player1.ball brick)
            |>List.unzip
            |>Tuple.second

        aftercollidebricks_2=aftercollidebricks_1
            |>List.map (\brick-> collideWith model.player2.ball brick)
            |>List.unzip
            |>Tuple.second

        (eliminated_1, rest)= 
            List.partition 
                (\brick-> Tuple.first(brick |> collideWith model.player1.ball) && brick.count<=0) 
                aftercollidebricks_2


        (eliminated_2, newrest)=
            List.partition
                (\brick-> Tuple.first(brick |> collideWith model.player2.ball) && brick.count<=0)
                rest

        audioList = 
            if List.isEmpty (eliminated_1 ++ eliminated_2)==False then
                "bgm/dice.wav"::model.audioList
            else 
                model.audioList

        player1 = model.player1
                |> tickState (time/1000)
                |> updateChosenCard
                |> catchHandcard
                |> movePaddle time
                |> moveBall model.state vaildBricks
                |> moveFallingcard
                |> addFallingcard eliminated_1
                

        player2 = model.player2
                |> tickState (time/1000)
                |> updateChosenCard
                |> catchHandcard
                |> movePaddle time
                |> moveBall model.state vaildBricks
                |> moveFallingcard
                |> addFallingcard eliminated_2
                
        (newPlayer1 , newPlayer2) = applySkill player1 player2
        

        bricks = moveBricks (newrest ++ invaildBricks)

        state = updateState player1 player2 model.state

    in
        { model
            | player1=newPlayer1
            , player2=newPlayer2
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


-- addFallingcard
addFallingcard eliminated player=
    {player | fallingcard = player.fallingcard ++ eliminated}
    


--move player's falling cards
moveFallingcard player=
    let
        fallingcards = List.map (\card-> {card|pos=nextPos card}) player.fallingcard
    in
        {player|fallingcard=fallingcards}

nextPos a= 
    addCoeffientVector 1 a.pos a.speed

addCoeffientVector: Float -> Vector Float -> Vector Float -> Vector Float
addCoeffientVector a x y=
    Vector (a*x.x+y.x) (a*x.y+y.y)




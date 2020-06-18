module Card exposing (catchHandcard,moveBricks,moveFallingcard)
import Paddle exposing (partPaddle)

import Model exposing (..)

isSkillCard brick =
    brick.suit >= 34

catchHandcard: Player -> Player
catchHandcard player =
    let
        (handcards,fallingcard)=
            List.partition
                    (\brick-> 
                        collideWithPaddle player.paddle brick.pos brick.size 
                        || collideWithPaddle (partPaddle player.paddle) brick.pos brick.size)
                    player.fallingcard 
        (skill, addingCard) = 
            List.partition
                (\brick ->
                    isSkillCard brick
                    ) handcards

        newPlayer = List.foldl dropCard player addingCard


        newHandcard = newPlayer.handcard
            |>List.sortBy .suit 
            |>List.map2 
                (\posx card->
                    {card | 
                        pos=Vector (posx+ handcardSetOff newPlayer.handcard) 0}
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
        , handcard = newHandcard
        , skill = skill}

moveBricks bricks =
    if List.any (\brick-> brick.pos.y >= (Model.attribute.bricksNum.y-1)*Model.brickHeight) bricks then
        bricks
    else
        List.map (\brick-> {brick| pos = nextPos brick}) bricks

moveFallingcard player=
    let
        fallingcards = List.map (\card-> {card|pos={x=card.pos.x,y=card.pos.y+1}}) player.fallingcard
    in
        {player|fallingcard=fallingcards}



collideWithPaddle paddle pos size=
    (paddle.pos.y|>inRange (pos.y+size.y) (pos.y+size.y+paddle.size.y+1))
  &&((nextPos paddle).x+paddle.size.x) > pos.x
  &&((nextPos paddle).x)< (pos.x+size.x)


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

inRange minx maxx x=
    minx<=x && x<=maxx

nextPos a= 
    addCoeffientVector 1 a.pos a.speed

addCoeffientVector: Float -> Vector Float -> Vector Float -> Vector Float
addCoeffientVector a x y=
    Vector (a*x.x+y.x) (a*x.y+y.y)
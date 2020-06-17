module Mahjong exposing (formHu)
import Model exposing(..)

formHu bricks =
    if List.length bricks == 0 then
        True
    else
        let 
            pongs = formPongs bricks  &&  formHu (List.drop 3 bricks)
            kong =  formKong bricks  &&  formHu (dropKong bricks)
            chow = if modBy 3 (List.length bricks) == 2 then
                        formChow bricks  &&  formHu (List.drop 2 bricks)
                    else 
                        False
        in
            pongs||kong||chow

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
                (List.member brick.suit (List.map .suit brick1))
                && (List.member brick.suit (List.map .suit brick2))
                



formKong bricks = 
    case (List.head bricks) of
        Nothing -> 
            False
        Just brick ->
            formKongHelper brick bricks

formKongHelper brick bricks =
    if vaildKong brick.suit then
        (List.member (brick.suit+1) (List.map .suit bricks))
        && (List.member (brick.suit+2) (List.map .suit bricks))
    else 
        False

vaildKong suit =
    suit < 27 &&  modBy 9 suit /=7  && modBy 9 suit /=8 



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

dropBrick suit bricks =
    let
        ( sameBricks,rest ) = List.partition (\brick -> brick.suit == suit) bricks
    in
        rest ++ (List.drop 1 sameBricks)

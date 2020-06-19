module Skill exposing (tickState,applySkill)

import Model exposing (..)
import Model exposing (PlayingState(..))

tickState time oldPlayer = 
    let
        player = if oldPlayer.taunted > 0 then
                { oldPlayer| taunted = (oldPlayer.taunted - time)}
            else 
                oldPlayer
    in
        case player.state of
            Spring lasttime ->
                if lasttime - time <= 0 then
                    player |> apply None
                else { player | state = Spring (lasttime - time)}
            Summer lasttime ->
                if lasttime - time <= 0 then
                    player |> apply None
                else { player | state = Summer (lasttime - time)}
            Autumn lasttime ->
                if lasttime - time <= 0 then
                    player |> apply None
                else { player | state = Autumn (lasttime - time)}

            Winter lasttime ->
                if lasttime - time <= 0 then
                    player |> apply None
                else { player | state = Winter (lasttime - time)}
            AllView lasttime ->
                if lasttime - time <= 0 then
                    player |> apply None
                else { player | state = AllView (lasttime - time)}
            None ->
                player

cancel player = 
    case player.state of
        Spring lasttime ->
            { player | state = None}
            |> setPaddleSize 0.8


        Summer lasttime ->
            { player | state = None}
            |> setBallSpeed  0.8
        Autumn lasttime ->
            { player | state = None}
            |> setFallingcardSpeed 0.8
            
        Winter lasttime ->
            { player | state = None}
            |> setBallSpeed  1.25
            |> setFallingcardSpeed 1.25
        AllView lasttime ->
            { player | state = None}
        None ->
            player


apply state player =
    let
        canceledPlayer = player|> cancel
    in 
        case state of
            Spring lasttime ->
                { canceledPlayer | state = state}
                |> setPaddleSize 1.25


            Summer lasttime ->
                { canceledPlayer | state = state}
                |> setBallSpeed  1.25
            Autumn lasttime ->
                { canceledPlayer | state = state}
                |> setFallingcardSpeed 1.25
            Winter lasttime ->
                { canceledPlayer | state = state}
                |> setBallSpeed  0.8
                |> setFallingcardSpeed 0.8
            AllView lasttime ->
                { canceledPlayer | state = state}
            None ->
                { canceledPlayer | state = state}


setFallingcardSpeed times player = 
    let 
        fallingcard = player.fallingcard
        newFallingcard = List.map (\brick -> { brick | speed = Vector (brick.speed.x*times) (brick.speed.y*times)}) fallingcard
    in
        { player|fallingcard = newFallingcard }

setBallSpeed times player = 
    let 
        ball = player.ball
        newBall = { ball | speed = Vector (ball.speed.x*times) (ball.speed.y*times)}
    in
        { player|ball = newBall }

applyCard brick player =
    case brick.suit of
        38 ->
            player |> apply (Spring 20)
        39 ->
            player |> apply (Summer 20)
        40 ->
            player |> apply (Autumn 20)
        41 ->
            player |> apply (Winter 20)
        _ ->
            player

setPaddleSize times player=
    let 
        paddle = player.paddle
        newPaddle = { paddle | size = Vector (paddle.size.x*times) paddle.size.y}
    in
        { player|paddle = newPaddle }


sendCard brick player =
    case brick.suit of
        34 ->
            player |> apply (Winter 20)
        35 ->
            player |> apply (AllView 5)
        36 ->
            setPaddleSize 1.2 player 
        37 ->
            setPaddleSize 0.8 player 
        43 ->
            { player| taunted =10 }
        _->
            player

applySkill player1 player2 =
    let
        sendedplayer2 = List.foldl sendCard player2 player1.skill
        sendedplayer1 = List.foldl sendCard player1 player2.skill
        applyedplayer1 = List.foldl applyCard sendedplayer1 player1.skill
        applyedplayer2 = List.foldl applyCard sendedplayer2 player2.skill

    in
        ( { applyedplayer1| skill = []}, {applyedplayer2| skill = []} )
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
            model |> start 

        Message.Rule -> 
            ( { model
                | state = Model.Rule 1
                , player1 = model.player1 |> setBallSpeed Model.attribute.defaultBallSpeed 
                , player2 = model.player2 |> setBallSpeed Model.attribute.defaultBallSpeed
              }
            , Random.generate NewBricks Model.randomList
            )

        Message.Story ->
            ( { model
                | state = Model.Story 1
              }
            , Cmd.none
            )

        NewBricks values->
            ( initBricks values model
            , Cmd.none
            )

        Move player moveDirection on->
            case player of
                Player1 ->
                    ({model|player1=model.player1 |> setPaddleAccelaration (Vector (( getDirection moveDirection on )*2) 0) },Cmd.none)
                Player2 ->
                    ({model|player2=model.player2 |> setPaddleAccelaration (Vector (( getDirection moveDirection on )*2) 0) },Cmd.none)

        MoveHandcard player moveDirection on->
            case player of
                Player1 ->
                    ({model|player1=model.player1 |> moveChosencard (getDirection moveDirection on) },Cmd.none)
                Player2 ->
                    ({model|player2=model.player2 |> moveChosencard (getDirection moveDirection on) },Cmd.none)

        Tick time ->
            case model.state of
                Playing ->
                    (model |> animate (min time 25) , Cmd.none )
                Paused ->
                    (model |> animate (min time 25) , Cmd.none )
                _ ->
                    (model , Cmd.none )

        Turn moveDirection ->
            case model.state of
                Model.Story n ->
                    if (n+moveDirection) <= 17 && (n+moveDirection) >=1 then
                        ({model | state = Model.Story (n+moveDirection)} , Cmd.none )
                    else if (n+moveDirection) <1 then ({model | state = Model.Paused} , Cmd.none )
                    else  ( model, Cmd.none )
                Model.Rule n->
                    if (n+moveDirection) <= 2 && (n+moveDirection) >=1 then
                        ({model | state = Model.Rule (n+moveDirection)} , Cmd.none )
                    else if (n+moveDirection) <1 then ({model | state = Model.Paused} , Cmd.none )
                    else ({model | state = Model.Story 16} , Cmd.none )
                    
                _ ->
                    ( model, Cmd.none )


                   
        ChangePlayersNumStart num ->
            model |> setPlayersNum num |> start 


                    
        ChangePlayersNum num->
            (model |> setPlayersNum num, Cmd.none )

        Noop ->
            ( model, Cmd.none )

start model=
    let
        (newModel , _)= (init ())
    in
    ( { newModel
            | state = Playing
            , player1 = newModel.player1 |> setBallSpeed Model.attribute.defaultBallSpeed 
            , player2 = newModel.player2 |> setBallSpeed Model.attribute.defaultBallSpeed
            }
        , Random.generate NewBricks Model.randomList
        )


setPlayersNum num model =
    let 
        attrs = model.attrs
        newAttrs = {attrs|playersNum = num}
    in
        {model | attrs = newAttrs}

getDirection moveDirection on =
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

setPaddleAccelaration: Vector Float -> Player -> Player--helper
setPaddleAccelaration accelaration player =
    let
        paddle = player.paddle
        newPaddle = {paddle| accelaration=accelaration}
    in
        {player|paddle=newPaddle}

setBallSpeed: Vector Float -> Player-> Player--helper
setBallSpeed speed player =
    let
        ball = player.ball
        newBall = {ball| speed=speed}
    in
        {player|ball=newBall}


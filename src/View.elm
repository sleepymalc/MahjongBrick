module View exposing (view)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style,src,controls,autoplay,loop)
import Html.Events exposing (on, onClick, onMouseDown, onMouseUp)

import Model exposing (..)
import Message exposing (Msg(..),PlayerNum(..))
import Paddle exposing (partPaddle)


view : Model -> Html Msg
view model =
    let
            renderHtml =
                case model.state of
                    Playing->
                        audio
                            [src "bgm/bgm1.mp3", autoplay True, loop True]
                            [Html.text "Your browser does not support the audio"]
                        :: List.map renderAudio model.audioList

                    Paused->
                        renderStart model
                    
                    Model.Rule -> 
                        renderRule model

                    Model.Story ->
                        renderStory model
                    Win player ->
                        renderOver model player

            renderSvg = 
                case model.state of
                    Playing ->
                        if model.attrs.playersNum == 2 then
                            [ svg
                                (transformedUI model.size (model.size.x/20))
                                (renderPlayerPlaying model.bricks model.player1)
                            , svg
                                (transformedUI model.size (model.size.x*3/20))
                                (renderPlayerPlaying model.bricks model.player2)
                            ]
                        else 
                            [ svg
                                (transformedUI model.size (model.size.x*3/10))
                                (renderPlayerPlaying model.bricks model.player2)
                            ]

                    Paused->
                        []
                    Model.Rule -> 
                        []

                    Model.Story -> 
                        [ svg
                            (transformedUI model.size (model.size.x/20))
                            [(renderImage ("img/blueTiger/Ming.png") (Vector 600 800) (Vector 0 0) [])
                            ]
                        , svg
                            (transformedUI model.size (model.size.x*3/20))
                            [(renderImage ("img/blueTiger/Gang.png") (Vector 600 800) (Vector 0 0) [])
                            ]
                        ]
                    Win player ->
                        case player of 
                            Player1 ->
                                [ svg
                                    (transformedUI model.size (model.size.x/20))
                                    (renderPlayerWin model.bricks model.player1)
                                , svg
                                    (transformedUI model.size (model.size.x*3/20))
                                    (renderPlayerLose model.bricks model.player2)
                                ]
                            Player2 ->
                                if model.attrs.playersNum == 2 then
                                    [ svg
                                        (transformedUI model.size (model.size.x/20))
                                        (renderPlayerLose model.bricks model.player1)
                                    , svg
                                        (transformedUI model.size (model.size.x*3/20))
                                        (renderPlayerWin model.bricks model.player2)
                                    ]
                                else
                                    [ svg
                                        (transformedUI model.size (model.size.x*3/10))
                                        (renderPlayerWin model.bricks model.player2)
                                    ]
    in
        div
            []
            [ span[]renderSvg
            , span[]renderHtml
            ]

renderState player = 
    let
        size = Vector brickWidth brickHeight
        pos = Vector (600-size.x) (800 - size.y)
    in
        case player.state of
            Spring _->
                [renderImage ("img/Monhjong/38.png") size pos []]
            Summer _->
                [renderImage ("img/Monhjong/39.png") size pos []]
            Autumn _->
                [renderImage ("img/Monhjong/40.png") size pos []]
            Winter _->
                [renderImage ("img/Monhjong/41.png") size pos []]
            AllView _->
                [renderImage ("img/Monhjong/35.png") size pos []]
            None ->
                []
            

         
renderRule model=[div[][Html.text "gkd! Rule!"]]
renderStory model =[]


gameUIAttribute size= 
    [ width (String.fromFloat (size.x*2/5))
    , height (String.fromFloat size.y)
    , viewBox "0 0 600 800"
    ]
transformedUI size setoff =
    transform ("translate("++(String.fromFloat setoff)++" 0)")
    :: (gameUIAttribute size)

renderAudio url =
    audio
        [src url, autoplay True]
        [Html.text "Your browser does not support the audio"]

renderPlayerPlaying bricks player = 
    renderbackground
    :: renderLogo
    :: renderBall  player.ball    
    :: renderPaddle player.paddle
    :: renderPaddle (partPaddle player.paddle)
    :: renderBricks (player.fallingcard ++ player.handcard)
    ++ renderState player
    ++ renderunBricks player (bricks)


renderPlayerWin bricks player =
    renderPlayerPlaying bricks player ++ [renderWin]
    
    


renderPlayerLose bricks player =
    renderPlayerPlaying bricks player ++ [renderLose]


renderWin =
    renderImage "img/blueTiger/win.png" (Vector (Model.attribute.range.x/2) (Model.attribute.range.y/2)) (Vector 0 (Model.attribute.range.y/2)) []



renderLose =
    renderImage "img/blueTiger/lose.png" (Vector (Model.attribute.range.x) (Model.attribute.range.y/2)) (Vector 0 (Model.attribute.range.y/2)) []


renderBall ball =
    let 
        ref = 
            if ball.speed == Vector 0 0 then 
                "img/dice/dice_"++(String.fromInt 1)++".png"
            else 
                "img/dice/dice_rolling_"++(String.fromInt (ball.imgIndex+1))++".png"
    in 
    renderImage ref ball.size ball.pos []
    

renderLogo =
    renderImage "img/logo.png" Model.attribute.range (Vector 0 0) []
    
renderPaddle paddle =
    rect
        [ x (String.fromFloat paddle.pos.x)
        , y (String.fromFloat paddle.pos.y)
        , width (String.fromFloat paddle.size.x)
        , height (String.fromFloat paddle.size.y)
        , fill "black"
        ]
        []
        


    
renderbackground =
    rect
        [ x "0"
        , y "0"
        , width (String.fromFloat Model.attribute.range.x)
        , height (String.fromFloat Model.attribute.range.y)
        , fill "#222F53"--"#A87272"--"#D3D3D3"--"#A87272"--"#D4A1A1"--"#D3D3D3"--
        ]
        []
    
renderunBrick player brick =
    let
        imgIndex = 
            case player.state of
                AllView _->
                    brick.suit+1
                _ -> 
                    if brick.count== Model.attribute.brickCount then
                        43
                    else
                        44
    in
    renderImage ("img/Monhjong/"++String.fromInt imgIndex++".png") brick.size (Vector brick.pos.x (brick.pos.y+10)) []

renderunBricks player bricks=
    bricks
    |> List.filter(\brick->brick.pos.y>=0)
    |> List.map (renderunBrick player)


renderBrick brick =
    renderImage ("img/Monhjong/"++String.fromInt (brick.suit+1)++".png") brick.size brick.pos []

    

renderBricks bricks =
    bricks
    |> List.filter (\brick->  brick.pos.y+brick.size.y >0)
    |> List.map renderBrick 

renderButton msg url size pos= 
    button
        [  Html.Attributes.style "border" "0"
        , Html.Attributes.style "bottom" "30px"
        , Html.Attributes.style "cursor" "pointer"
        , Html.Attributes.style "display" "block"
        , Html.Attributes.style "height" "60px"
        , Html.Attributes.style "left" "30px"
        , Html.Attributes.style "line-height" "60px"
        , Html.Attributes.style "outline" "none"
        , Html.Attributes.style "padding" "0"
        -- Display at center
        , Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" ((String.fromFloat pos.x)++"px")
        , Html.Attributes.style "top" ((String.fromFloat pos.y)++"px")
        -- 
        , Html.Attributes.style "width" "120px"
        , onClick msg
        ]
        [ Html.img [src url
        , height ((String.fromFloat size.y)++"px") 
        , width ((String.fromFloat size.x)++"px")][] ]

renderStart model=
    let
        size = Vector 120 60
    in
    [ renderButton Start "img/button/start.png" size (Vector (model.size.x/2-60) (model.size.y/2-100))
    , renderButton Message.Story "img/button/story.png" size (Vector (model.size.x/2-60) (model.size.y/2-20))
        
    , renderButton Message.Rule "img/button/rule.png" size (Vector (model.size.x/2-60) (model.size.y/2+60))
    , if model.attrs.playersNum == 1 then
        renderButton (ChangePlayersNum 2) "img/button/rule.png" size (Vector (model.size.x/2-60) (model.size.y/2+140))
        else 
        renderButton (ChangePlayersNum 1) "img/button/rule.png" size (Vector (model.size.x/2-60) (model.size.y/2+140))
    ]

renderImage url size pos attr=
    Svg.image
        ([  xlinkHref url
        , width (String.fromFloat size.x)--(String.fromFloat 40.2)
        , height (String.fromFloat size.y)--(String.fromFloat 52.8)
        , x (String.fromFloat pos.x)
        , y (String.fromFloat pos.y)
        ]
        ++ attr)
    []
renderOver model player= renderStart model


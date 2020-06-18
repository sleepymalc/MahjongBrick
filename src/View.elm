module View exposing (view)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style,src,controls,autoplay,loop)
import Html.Events exposing (on, onClick, onMouseDown, onMouseUp)

import Model exposing (Model, attribute,State(..))
import Message exposing (Msg(..),PlayerNum(..))
import Model exposing (Vector)
import Paddle exposing (partPaddle)
import Model exposing (PlayingState(..))


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
                        [ svg
                            (transformedUI model.size Player1)
                            (renderPlayerPlaying model.bricks model.player1)
                        , svg
                            (transformedUI model.size Player2)
                            (renderPlayerPlaying model.bricks model.player2)
                        ]
                    Paused->
                        []
                    Model.Rule -> 
                        []

                    Model.Story -> 
                        []
                    Win player ->
                        case player of 
                            Player1 ->
                                [ svg
                                    (transformedUI model.size Player1)
                                    (renderPlayerWin model.bricks model.player1)
                                , svg
                                    (transformedUI model.size Player2)
                                    (renderPlayerLose model.bricks model.player2)
                                ]
                            Player2 ->
                                [ svg
                                    (transformedUI model.size Player1)
                                    (renderPlayerWin model.bricks model.player1)
                                , svg
                                    (transformedUI model.size Player2)
                                    (renderPlayerLose model.bricks model.player2)
                                ]
    in
        div
            []
            [ span[]renderSvg
            , span[]renderHtml
            ]
            
renderRule model=[]
renderStory model =[]
gameUIAttribute size= 
    [ width (String.fromFloat (size.x*2/5))
    , height (String.fromFloat size.y)
    , viewBox "0 0 600 800"
    ]
transformedUI size player =
    case player of
        Player1 ->
            transform ("translate("++(String.fromFloat (size.x/20))++" 0)")
            :: (gameUIAttribute size)
        Player2 ->
            transform ("translate("++(String.fromFloat (size.x*3/20))++" 0)")
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


renderStart model=
    [button
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
        , Html.Attributes.style "left" ((String.fromFloat (model.size.x/2-60))++"px")
        , Html.Attributes.style "top" ((String.fromFloat (model.size.y/2-100))++"px")
        -- 
        , Html.Attributes.style "width" "120px"
        , onClick Start
        ]
        [ Html.img [src "img/button/start.png", height "60px" , width "120px"][] ]
    , button
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
        , Html.Attributes.style "left" ((String.fromFloat (model.size.x/2-60))++"px")
        , Html.Attributes.style "top" ((String.fromFloat (model.size.y/2-30))++"px")
        -- 
        , Html.Attributes.style "width" "120px"
        , onClick Message.Story
        ]
        [ Html.img [src "img/button/story.png", height "60px" , width "120px"][] ]
    , button
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
        , Html.Attributes.style "left" ((String.fromFloat (model.size.x/2-60))++"px")
        , Html.Attributes.style "top" ((String.fromFloat (model.size.y/2+40))++"px")
        -- 
        , Html.Attributes.style "width" "120px"
        , onClick Message.Rule
        ]
        [ Html.img [src "img/button/rule.png", height "60px" , width "120px"][] ]]

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
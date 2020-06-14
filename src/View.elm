module View exposing (view)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style,src,controls,autoplay,loop)
import Html.Events exposing (on, onClick, onMouseDown, onMouseUp)

import Model exposing (Model, attribute,State(..))
import Message exposing (Msg(..))


view : Model -> Html Msg
view model =
    let
            gameUIAttribute = 
                [ width (String.fromFloat (model.size.x*2/5))
                , height (String.fromFloat model.size.y)
                , viewBox "0 0 600 800"
                ]
            renderHtml =
                case model.state of
                    Playing->
                        List.append
                        [audio
                            [src "bgm/1.mp3", (autoplay True)]
                            [Html.text "Your browser does not support the audio"]
                        ,audio
                            [src "bgm/bgm1.mp3", (autoplay True), (loop True)]
                            [Html.text "Your browser does not support the audio"]
                        ]
                        (List.map renderAudio model.audioList)

                    Paused->
                        [renderStart model]
    in
        div
            []
            [ svg
                (transform ("translate("++(String.fromFloat (model.size.x/20))++" 0)")
                :: gameUIAttribute)
                (renderPlayer model.bricks model.player1)
            , svg
                (transform ("translate("++(String.fromFloat (model.size.x*3/20))++" 0)")
                :: gameUIAttribute)
                (renderPlayer model.bricks model.player2)
            , span[]renderHtml
            ]
            

renderAudio url =
    audio
        [src url, (autoplay True)]
        [Html.text "Your browser does not support the audio"]

renderPlayer bricks player = 
    renderbackground
    :: renderFrame
    :: renderLogo
    :: renderPaddle player.paddle
    :: renderBall  player.ball
    :: renderBricks (List.append (List.append bricks player.fallingcard) player.handcard)


renderBall ball =
    let 
        ref = 
            if ball.speed == {x=0,y=0} then 
                "img/dice/dice_"++(String.fromInt 1)++".png"
            else 
                "img/dice/dice_rolling_"++(String.fromInt (ball.imgIndex+1))++".png"
    in 
    Svg.image
        [   xlinkHref ref
        ,   width (String.fromFloat ball.size.x)
        ,   height (String.fromFloat ball.size.y)
        ,   x (String.fromFloat ball.pos.x)
        ,   y (String.fromFloat ball.pos.y)
        ]
    []

renderLogo =
    Svg.image
        [  xlinkHref "img/logo.png"
        , x "0"
        , y "0"
        , width (String.fromFloat Model.attribute.range.x)
        , height (String.fromFloat Model.attribute.range.y)
        ]
    []
renderPaddle paddle =
    rect
        [ x (String.fromFloat paddle.pos.x)
        , y (String.fromFloat paddle.pos.y)
        , width (String.fromFloat paddle.size.x)
        , height (String.fromFloat paddle.size.y)
        , rx "10"
        , ry "10"
        , fill "black"
        ]
        []


    
renderbackground =
    rect
        [ x "0"
        , y "0"
        , width (String.fromFloat Model.attribute.range.x)
        , height (String.fromFloat Model.attribute.range.y)
        , rx "10"
        , ry "10"
        , fill "#222F53"
        ]
        []
    
renderFrame =
    Svg.image
        [ xlinkHref "img/frame.png"
        , x "0"
        , y "0"
        , width (String.fromFloat Model.attribute.range.x)
        , height (String.fromFloat Model.attribute.range.y)
        ]
    []
renderBrick brick =
    Svg.image
        [  xlinkHref ("img/suit/"++(String.fromInt (brick.suit//4+1))++".png")
        , width (String.fromFloat brick.size.x)--(String.fromFloat 40.2)
        , height (String.fromFloat brick.size.y)--(String.fromFloat 52.8)
        , x (String.fromFloat brick.pos.x)
        , y (String.fromFloat brick.pos.y)
        , rx "1"
        , ry "1"
        ]
    []
    

renderBricks bricks =
    bricks
    |> List.filter (\brick-> brick.pos.y>=0)
    |> List.map renderBrick 


renderStart model=
            button
                [ Html.Attributes.style "background" "#34495f"
                , Html.Attributes.style "border" "0"
                , Html.Attributes.style "bottom" "30px"
                , Html.Attributes.style "color" "#fff"
                , Html.Attributes.style "cursor" "pointer"
                , Html.Attributes.style "display" "block"
                , Html.Attributes.style "font-family" "Helvetica, Arial, sans-serif"
                , Html.Attributes.style "font-size" "18px"
                , Html.Attributes.style "font-weight" "300"
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
                , onClick Start
                ]
                [ Html.text "Start" ]
module View exposing (view)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style,src,controls,autoplay,loop)
import Html.Events exposing (on, onClick, onMouseDown, onMouseUp)

import Model exposing (Model, attribute,State(..))
import Message exposing (Msg(..))
import Model exposing (Vector)


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
                        audio
                            [src "bgm/bgm1.mp3", autoplay True, loop True]
                            [Html.text "Your browser does not support the audio"]
                        :: List.map renderAudio model.audioList

                    Paused->
                        [renderStart model]
                    Win player ->
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
        [src url, autoplay True]
        [Html.text "Your browser does not support the audio"]

renderPlayer bricks player = 
    renderbackground
    :: renderLogo
    :: renderPaddle player.paddle
    :: renderBall  player.ball    
    :: renderBricks (player.fallingcard ++ player.handcard)
    ++ renderunBricks (bricks)

renderBall ball =
    let 
        ref = 
            if ball.speed == {x=0,y=0} then 
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
    
renderunBrick brick =
    renderImage "img/suit/43.jpeg" brick.size brick.pos [opacity (String.fromFloat ((toFloat brick.count) / 4))]

renderunBricks bricks=
    bricks
    |> List.filter(\brick->brick.pos.y>=0)
    |> List.map renderunBrick


renderBrick brick =
   renderImage ("img/suit/"++String.fromInt ((brick.suit-1)//4+1)++".jpeg") brick.size brick.pos []


    

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
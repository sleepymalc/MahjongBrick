module View exposing (view)

import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style,src,controls,autoplay,loop,attribute)
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

                    Model.Story n->
                        renderStory model n
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

                    Paused->[]--[ svg(transformedUI model.size (model.size.x*3/10))[renderLogo]]
                    Model.Rule -> 
                        []

                    Model.Story n-> 
                        []
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

viewAttrs = []


renderState player = 
    let
        size = Vector brickWidth brickHeight
        pos = Vector (600-size.x) (800 - size.y)
    in
        case player.state of
            Spring _->
                [renderImage ("img/suit/38.png") size pos []]
            Summer _->
                [renderImage ("img/suit/39.png") size pos []]
            Autumn _->
                [renderImage ("img/suit/40.png") size pos []]
            Winter _->
                [renderImage ("img/suit/41.png") size pos []]
            AllView _->
                [renderImage ("img/suit/35.png") size pos []]
            None ->
                []
            

         
renderRule model=[div[][img [width (String.fromFloat model.size.x), height (String.fromFloat model.size.y) ,src "img/rule.jpeg"] []]]
renderStory model n=[
    div[]
    [ img [width (String.fromFloat model.size.x)
    , height (String.fromFloat model.size.y) 
    , src ("img/dialog/"++(String.fromFloat (toFloat n))++".png")
    ] []]]


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
    renderImage "img/paddle3.png" paddle.size paddle.pos []
    {-rect
        [ x (String.fromFloat paddle.pos.x)
        , y (String.fromFloat paddle.pos.y)
        , width (String.fromFloat paddle.size.x)
        , height (String.fromFloat paddle.size.y)
        , fill "black"
        ]
        []-}
        


    
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
    renderImage ("img/suit/"++String.fromInt imgIndex++".png") brick.size (Vector brick.pos.x (brick.pos.y+10)) []

renderunBricks player bricks=
    bricks
    |> List.filter(\brick->brick.pos.y>=0)
    |> List.map (renderunBrick player)


renderBrick brick =
    renderImage ("img/suit/"++String.fromInt (brick.suit+1)++".png") brick.size brick.pos []

    

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
    , renderChoosePlayer model.attrs.playersNum size (Vector (model.size.x/2-60) (model.size.y/2+140))]
    

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

stylesheet =
    let
        tag =
            "link"

        attrs = 
            [ Html.Attributes.attribute "Rel" "stylesheet"
            , Html.Attributes.attribute "property" "stylesheet"
            , Html.Attributes.attribute "href" "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
            ]
        
        children =
            []
    in
        Html.node tag attrs children

renderChoosePlayer chosen size pos= 
    let
        (attr1, attr2)= 
            if chosen == 1 then
                ( [ class "btn btn-primary"]
                , [ class "btn btn-second"
                  , onClick (Message.ChangePlayersNum 2)
                ])
            else
                ( [ class "btn btn-second"
                  , onClick (Message.ChangePlayersNum 1)]
                , [ class "btn btn-primary"
                ])
        attrs = 
            [ Html.Attributes.style "left" ((String.fromFloat pos.x)++"px")
            , Html.Attributes.style "top" ((String.fromFloat pos.y)++"px")
            , Html.Attributes.style "width" "80px"
            , Html.Attributes.style "height" "40px"]
        

    in
    div [ class "btn-group"]
    [
        stylesheet
        ,
        button 
        (attrs ++ attr1)
        [ Html.text "1-Player"]
        ,
        button 
        (attrs ++ attr2)
        [ Html.text "2-Players"]
        
    ]


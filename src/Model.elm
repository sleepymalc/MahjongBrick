module Model exposing(..)
import Message exposing (Msg(..),MoveDirection(..),PlayerNum(..))
import Browser.Dom exposing (getViewport)
import Task
import Svg.Attributes exposing (speed)
import List exposing (range)
import Svg exposing (a)
import Json.Encode exposing (int)
import Random
import Html.Attributes exposing (value)
import Dict exposing (values)
import Json.Decode exposing (Value)
type alias Vector a=
    { x:a
    , y:a
    }

type alias Size =
    Vector Float

type alias Pos =
    Vector Float

type alias Speed =
    Vector Float

type alias Brick =
    { suit: Int
    , size: Size
    , pos: Pos
    , count: Int
    }

type State
    = Paused
    | Playing
    | Win PlayerNum

type alias Ball =
    { size: Size
    , pos: Pos
    , speed: Speed
    , imgIndex: Int
    , punish: Bool
    }

type alias Paddle =
    { size: Size
    , pos: Pos
    , speed: Float
    }

type alias Background =
    { size: Size
    , pos: Pos
    }

type alias Player =
    { paddle: Paddle
    , ball: Ball
    , handcard: List Brick
    , chosen: Int
    , skill: List Brick
    , fallingcard: List Brick 
    , chosenCard: Int 
    , moveHandcard: Int
    , droppedcard: List Brick 
    }
type alias Model =
    { player1: Player
    , player2: Player 
    , bricks: List Brick
    , state: State
    , size: Size
    , audioList: List String
    --background : Background
    }

attribute =
    { playersNum = 2
    , range = Vector 600 800
    , bricksNum = Vector 12 3--need change?
    , totalBricksNum = 144
    , defaultBallSpeed =Vector 3 -2
    , handcardPosY = 650
    , handcardNum = 13
    }

    
init : () -> (Model, Cmd Msg)
init _= 
    ({ player1 = initPlayer
    , player2 = initPlayer
    , bricks = []
    , state = Paused--to be update
    , size = Vector 0 0
    , audioList = []
    --,{ background = { width=widthRange, height= heightRange, pos={x=0,y=0}}
    },Task.perform GetViewport getViewport)


generateRow  suit y =
    List.map (\x-> {suit=suit, size = Vector brickWidth brickHeight, pos = Vector x y ,count=4}) (posXList attribute.bricksNum.x)

brickWidth = attribute.range.x/attribute.bricksNum.x
brickHeight = attribute.range.y/4/attribute.bricksNum.y
posXList n= 
    (List.range 0 (n-1)
        |> List.map (\x-> ((toFloat x))*brickWidth))
posYList = List.range (-attribute.bricksNum.y+1) (attribute.totalBricksNum//attribute.bricksNum.x-attribute.bricksNum.y)
            |> List.map (\x-> -((toFloat x))*brickHeight)


--randomList: Random.Generator (List Int)
randomList =
    Random.list attribute.totalBricksNum (Random.int 0 100000)
    
--initBricks: List Int->List Brick
initBricks values model= 
    let
        bricks = List.map (generateRow 1) posYList
                |> List.concat
                |> List.map2 
                    (\value brick ->
                        {brick|suit=value}
                    ) values 
                |> List.sortBy .suit
                |> List.indexedMap 
                    (\index brick->
                        {brick|suit=index}
                    )
                |> List.sortBy (\brick ->  brick.pos.y * 10000+ brick.pos.x)

        (player1, rest) = deal bricks model.player1
        (player2, newrest) = deal rest model.player2
    in
        { model
        | player1 = player1
        , player2 = player2
        , bricks = newrest}


deal bricks player =
    let 
        handcard = List.take attribute.handcardNum bricks
        newBricks = List.drop attribute.handcardNum bricks
    in 
        ({player|handcard = handcard}, newBricks)
    

initPlayer : Player
initPlayer = 
    { paddle = initPaddle
    , ball = initBall
    , handcard = []
    , chosen = 0
    , skill = []
    , fallingcard = []
    , chosenCard = 0
    , moveHandcard = 0
    , droppedcard = []
    }
    
initPaddle : Paddle
initPaddle =
    {  size = {x = 200, y = 20}
    , pos = Vector (attribute.range.x/2-200/2) (attribute.range.y*2/3+25) 
    , speed = 0
    }

initBall : Ball
initBall =
    { size = {x = 20, y = 20}
    , pos = Vector (attribute.range.x/2-30/2) (attribute.range.y*2/3) 
    , speed = attribute.defaultBallSpeed
    , punish = False
    , imgIndex=0
    }

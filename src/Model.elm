module Model exposing
    ( Model
    , Vector
    , Brick
    , State(..)
    , generateRow
    , init
    , initBricks
    , attribute
    , brickWidth
    , brickHeight
    , posXList
    , posYList
    )

import Message exposing (Msg(..),MoveDirection(..))
import Browser.Dom exposing (getViewport)
import Task
import Svg.Attributes exposing (speed)
import List exposing (range)
import Svg exposing (a)
import Json.Encode exposing (int)
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
    }

type State
    = Paused
    | Playing

type alias Ball =
    { size: Size
    , pos: Pos
    , speed: Speed
    , imgIndex: Int
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
    {   paddle: Paddle
       ,ball: Ball
       ,handcard: List Brick
       ,skill: List Brick
       ,fallingcard: List Brick 
    }
type alias Model =
    { player1: Player
    , player2: Player 
    , bricks: List Brick
    , state: State
    , size: Size
    --background : Background
    }

attribute =
    { playersNum = 2
    , range = Vector 600 800
    , bricksNum = Vector 13 3--need change?
    , defaultBallSpeed =Vector 3 -2
    , handcardPosY = 600
    }

    
init : () -> (Model, Cmd Msg)
init _= 
    ({ player1 = initPlayer
    , player2 = initPlayer
    , bricks = initBricks
    , state = Paused--to be update
    , size = Vector 0 0
    --,{ background = { width=widthRange, height= heightRange, pos={x=0,y=0}}
    },Task.perform GetViewport getViewport)


generateRow  suit y =
    List.map (\x-> {suit=suit, size = Vector brickWidth brickHeight, pos = Vector x y }) posXList

brickWidth = attribute.range.x/attribute.bricksNum.x
brickHeight = attribute.range.y/4/attribute.bricksNum.y
posXList = 
    (List.range 0 (attribute.bricksNum.x-1)
        |> List.map (\x-> ((toFloat x))*brickWidth))
posYList = List.range 0 (attribute.bricksNum.y-1)
            |> List.map (\x-> ((toFloat x))*brickHeight)
initBricks: List Brick
initBricks = 
    let

        bricks=List.map (generateRow 1) posYList
    in
        List.concat bricks
    

initPlayer : Player
initPlayer = 
    { paddle = initPaddle
    , ball = initBall
    , handcard = []
    , skill = []
    , fallingcard = []
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
    , imgIndex=0
    }
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
    , speed: Speed
    }

type State
    = Paused
    | Rule
    | Story Int
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
    , speed: Speed
    , accelaration: Vector Float
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
    , state: PlayingState
    , taunted: Float
    }
type PlayingState =
    Spring Float
    | Summer Float
    | Autumn Float
    | Winter Float
    | AllView Float
    | None


type alias Model =
    { player1: Player
    , player2: Player 
    , bricks: List Brick
    , state: State
    , size: Size
    , audioList: List String
    , attrs: CustomAttribute
    --, view: Bool
    --background : Background
    }

type alias CustomAttribute =
    { playersNum: Int
    , handcardNum: Int
    }

attribute =
    { range = Vector 600 800
    , bricksNum = Vector 13 3--need change?
    , totalBricksNum = 43 * 4
    , defaultBallSpeed =Vector 3 -2
    , handcardPosY = 650
    , brickCount = 4
    , paddleAccelaration = 0.01---1 in Update
    , brickSpeed = 1
    , maxPaddleSpeedX = 4
    , fiction = Vector 1 0
    , paddleSize = Vector 200 20
    }

    
init : () -> (Model, Cmd Msg)
init _= 
    ({ player1 = initPlayer
    , player2 = initPlayer
    , bricks = []
    , state = Paused
    , size = Vector 0 0
    , audioList = []
    , attrs = initAttrs
    --,{ background = { width=widthRange, height= heightRange, pos={x=0,y=0}}
    },Task.perform GetViewport getViewport)


generateRow  suit y =
    List.map (\x-> 
        { suit=suit
        , size = Vector brickWidth brickHeight
        , pos = Vector x y 
        , count=attribute.brickCount
        , speed = Vector 0 attribute.brickSpeed
        }) (posXList attribute.bricksNum.x)

brickWidth = attribute.range.x/attribute.bricksNum.x
brickHeight = attribute.range.y/4/attribute.bricksNum.y
posXList n= 
    (List.range 0 (n-1)
        |> List.map (\x-> ((toFloat x))*brickWidth))
posYList = List.range (-attribute.bricksNum.y+1) (attribute.totalBricksNum//attribute.bricksNum.x-attribute.bricksNum.y)
            |> List.map (\x-> -((toFloat x))*(brickHeight))


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
                        {brick|suit=(value-1)//4}
                    ) values 
                |> List.sortBy .suit
                |> List.indexedMap 
                    (\index brick->
                        {brick|suit=(index-1)//4}
                    )
                |> List.sortBy (\brick ->  brick.pos.y * 10000+ brick.pos.x)

        (player1, rest) = deal model.attrs.handcardNum bricks model.player1 
        (player2, newrest) = deal model.attrs.handcardNum rest model.player2
    in
        { model
        | player1 = player1
        , player2 = player2
        , bricks = newrest}


deal handcardNum bricks player =
    let 
        handcard = List.take handcardNum bricks
        newBricks = List.drop handcardNum bricks
    in 
        ({player|handcard = handcard}, newBricks)

initAttrs : CustomAttribute
initAttrs = 
    { playersNum = 2
    , handcardNum= 13
    }

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
    , state = None
    , taunted = 0
    }
    
initPaddle : Paddle
initPaddle =
    { size = attribute.paddleSize
    , pos = Vector (attribute.range.x/2-200/2) (attribute.range.y*2/3+25) 
    , speed = Vector 0 0
    , accelaration = Vector 0 0
    }

initBall : Ball
initBall =
    { size = Vector 20 20
    , pos = Vector (attribute.range.x/2-30/2) (attribute.range.y*2/3) 
    , speed = attribute.defaultBallSpeed
    , punish = False
    , imgIndex=0
    }

module Message exposing (Msg(..),MoveDirection(..),PlayerNum(..))
import Browser.Dom exposing (Viewport)

type MoveDirection = Left | Right
type PlayerNum = Player1|Player2


type Msg
    = Move PlayerNum MoveDirection Bool
    | Resize Int Int
    | Tick Float
    | Noop
    | GetViewport Viewport
    | Start
    | Rule
    | Story
    | NewBricks (List Int)
    | MoveHandcard PlayerNum MoveDirection Bool
    | ChangePlayersNum Int
    | ChangePlayersNumStart Int
    | Turn Int
module Message exposing (Msg(..),MoveDirection(..),PlayerNum(..))
import Browser.Dom exposing (Viewport)
type MoveDirection = Left | Right | Up | Down
type PlayerNum = Player1|Player2
type Msg
    = Move PlayerNum MoveDirection Bool
    | Resize Int Int
    | Tick Float
    | Noop
    | GetViewport Viewport
    | Start
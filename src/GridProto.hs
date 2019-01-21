{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
module GridProto
  ( Map, fromList, lookup, (!), delete, alter, insert, filterWithKey, member, notMember, toList
  , Color(..)
  , Shape(..)
  , Input(..)
  , Mouse(..)
  , Key(..)
  , KeyState(..)
  , Cell(..)
  , GridProto(..)
  , runGridProto
  ) where

import Prelude hiding (lookup)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map, fromList, lookup, (!), delete, alter, insert, filterWithKey, member, notMember, toList)
import Data.Function (fix)
import Data.Foldable (forM_)
import Data.Text (pack)
import Data.Word (Word8)
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))

import qualified SDL
import qualified SDL.Primitive as Gfx
import qualified Data.Vector.Storable as VS

data Color
  = Red
  | LiteRed
  | DarkRed
  | Green
  | LiteGreen
  | DarkGreen
  | Blue
  | LiteBlue
  | DarkBlue
  | Yellow
  | LiteYellow
  | DarkYellow
  | Magenta
  | LiteMagenta
  | DarkMagenta
  | Cyan
  | LiteCyan
  | DarkCyan
  | Orange
  | LiteOrange
  | DarkOrange
  | Pink
  | LitePink
  | DarkPink
  | Brown
  | LiteBrown
  | DarkBrown
  | White
  | Gray
  | LiteGray
  | DarkGray
  | Black
  deriving (Enum, Eq, Bounded, Show, Generic)

instance ToJSON Color
instance FromJSON Color

data Shape
  = Circle
  | FillCircle
  | Triangle
  | FillTriangle
  | Square
  | FillSquare
  | Plus
  | Minus
  | X
  deriving (Enum, Eq, Bounded, Show, Generic)

instance ToJSON Shape
instance FromJSON Shape

data Mouse
  = Hover (Int, Int)
  | Click (Int, Int)
  | Idle
  deriving (Show, Eq, Generic)

instance ToJSON Mouse
instance FromJSON Mouse

data Key
  = Char Char
  | UpArrow
  | DownArrow
  | LeftArrow
  | RightArrow
  | Enter
  | Shift
  | Ctrl
  | AltKey
  | Tab
  | Backspace
  | Meta
  deriving (Eq, Show, Generic)

instance ToJSON Key
instance FromJSON Key

data KeyState
  = Pressed
  | Held
  | Released
  deriving (Enum, Eq, Bounded, Show, Generic)

instance ToJSON KeyState
instance FromJSON KeyState

data Input = Input
  { mouse :: Mouse
  , keys :: [(Key, KeyState)]
  } deriving (Show, Eq, Generic)

instance ToJSON Input
instance FromJSON Input

data Cell = Cell
  { shape :: Maybe (Shape, Color)
  , fill :: Color
  } deriving (Show, Eq, Generic)

instance ToJSON Cell
instance FromJSON Cell

data GridProto s = GridProto
  { title :: String
  , rows :: Int
  , cols :: Int
  , cellPixelSize :: Int
  , setupFn :: IO s
  , updateFn :: Input -> s -> IO s
  , cleanupFn :: s -> IO ()
  , cellsFn :: s -> Map (Int, Int) Cell
  , quitFn :: s -> Bool
  }

runGridProto :: GridProto s -> IO ()
runGridProto GridProto
  { title
  , rows
  , cols
  , cellPixelSize
  , setupFn
  , updateFn
  , cellsFn
  , quitFn
  }
  = do
  SDL.initialize [SDL.InitVideo, SDL.InitAudio]
  window <- SDL.createWindow (pack title) SDL.defaultWindow { SDL.windowInitialSize = V2 (fromIntegral $ rows * cellPixelSize) (fromIntegral $ cols * cellPixelSize) }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  initialState <- setupFn
  ($ initialState) $ fix $ \loop state -> do
    let quit = quitFn state
    events <- SDL.pollEvents
    (SDL.P mousePos) <- SDL.getAbsoluteMouseLocation
    let (V2 mouseX mouseY) = fromIntegral <$> mousePos
    let mouseCellPos = cellByMousePosition cellPixelSize (mouseX, mouseY) (rows, cols)
    mouseClick <- ($ SDL.ButtonLeft) <$> SDL.getMouseButtons
    let input = makeInput mouseCellPos mouseClick events
    if quit || elem SDL.QuitEvent (map SDL.eventPayload events)
      then return ()
      else do
        state' <- updateFn input state
        let cellMap = cellsFn state'
        SDL.clear renderer
        drawCellMap renderer cellPixelSize cellMap
        SDL.present renderer
        loop state'
  SDL.destroyWindow window
  SDL.quit

makeInput :: Maybe (Int, Int) -> Bool -> _a -> Input
makeInput mpos' mclick _ = Input m []
  where
    m = case mpos' of
      Nothing -> Idle
      Just mpos -> (if mclick then Click else Hover) mpos

drawCellMap :: SDL.Renderer -> Int -> Map (Int, Int) Cell -> IO ()
drawCellMap renderer cellSize m = forM_ (toList m) $ \((x,y), Cell{shape,fill}) -> do
  drawFill renderer cellSize (x,y) fill
  case shape of
    Nothing -> return ()
    Just shape' -> drawShape renderer cellSize (x,y) shape'

drawFill :: SDL.Renderer -> Int -> (Int, Int) -> Color -> IO ()
drawFill renderer cellSize (x,y) color = do
  let fx0 = x * cellSize
      fx1 = (x + 1) * cellSize
      fy0 = y * cellSize
      fy1 = (y + 1) * cellSize
  Gfx.fillRectangle
    renderer
    (V2 (fromIntegral fx0) (fromIntegral fy0))
    (V2 (fromIntegral fx1) (fromIntegral fy1))
    (colorPixel color)

drawShape :: SDL.Renderer -> Int -> (Int,  Int) -> (Shape, Color) -> IO ()
drawShape renderer cellSize (x,y) (shape,color) = case shape of
  --
  Circle -> return ()
  --
  FillCircle -> return ()
  --
  Triangle -> return ()
  --
  FillTriangle -> return ()
  --
  Square -> let
    fx0 = x * cellSize + thickness
    fx1 = (x + 1) * cellSize - thickness
    fy0 = y * cellSize + thickness
    fy1 = (y + 1) * cellSize - thickness
    in Gfx.rectangle
        renderer
        (V2 (fromIntegral fx0) (fromIntegral fy0))
        (V2 (fromIntegral fx1) (fromIntegral fy1))
        (colorPixel color)
  --
  FillSquare -> let
    fx0 = x * cellSize + thickness
    fx1 = (x + 1) * cellSize - thickness
    fy0 = y * cellSize + thickness
    fy1 = (y + 1) * cellSize - thickness
    in Gfx.fillRectangle
        renderer
        (V2 (fromIntegral fx0) (fromIntegral fy0))
        (V2 (fromIntegral fx1) (fromIntegral fy1))
        (colorPixel color)
  --
  Plus -> return ()
  --
  Minus -> return ()
  --
  X -> return ()
  where
    thickness :: Int
    thickness = max (cellSize `div` 8) 1

colorPixel :: Color -> Gfx.Color
colorPixel c = bgr (colorValue c)

bgr :: (Word8, Word8, Word8) -> Gfx.Color
bgr (r,g,b) = V4 (fromIntegral r) (fromIntegral g) (fromIntegral b) 0xff

colorValue :: Integral a => Color -> (a, a, a)
colorValue Red         = (0xff, 0x00, 0x00)
colorValue LiteRed     = (0xff, 0x44, 0x44)
colorValue DarkRed     = (0xaa, 0x00, 0x00)
colorValue Green       = (0x00, 0xff, 0x00)
colorValue LiteGreen   = (0x44, 0xff, 0x44)
colorValue DarkGreen   = (0x00, 0xaa, 0x00)
colorValue Blue        = (0x00, 0x00, 0xff)
colorValue LiteBlue    = (0x44, 0x44, 0xff)
colorValue DarkBlue    = (0x00, 0x00, 0xaa)
colorValue Yellow      = (0xff, 0xff, 0x00)
colorValue LiteYellow  = (0xff, 0xff, 0x44)
colorValue DarkYellow  = (0xaa, 0xaa, 0x00)
colorValue Magenta     = (0xff, 0x00, 0xff)
colorValue LiteMagenta = (0xff, 0x44, 0xff)
colorValue DarkMagenta = (0xaa, 0x00, 0xaa)
colorValue Cyan        = (0x00, 0xff, 0xff)
colorValue LiteCyan    = (0x44, 0xff, 0xff)
colorValue DarkCyan    = (0x00, 0xaa, 0xaa)
colorValue Orange      = (0xff, 0x7f, 0x00)
colorValue LiteOrange  = (0xff, 0xaf, 0x33)
colorValue DarkOrange  = (0xcf, 0x4f, 0x00)
colorValue Pink        = (0xff, 0x66, 0x99)
colorValue LitePink    = (0xff, 0x99, 0xcc)
colorValue DarkPink    = (0xaa, 0x22, 0x44)
colorValue Brown       = (0x88, 0x44, 0x00)
colorValue LiteBrown   = (0xaa, 0x77, 0x44)
colorValue DarkBrown   = (0x55, 0x22, 0x00)
colorValue Black       = (0x00, 0x00, 0x00)
colorValue Gray        = (0x77, 0x77, 0x77)
colorValue LiteGray    = (0xaa, 0xaa, 0xaa)
colorValue DarkGray    = (0x44, 0x44, 0x44)
colorValue White       = (0xff, 0xff, 0xff)

-- symbolList :: [Char]
-- symbolList = "`1234567890-=~!@#$%^&*()_+qwertyuiop[]\\QWERTYUIOP{}|asdfghjkl;'ASDFGHJKL:\"zxcvbnm,./ZXCVBNM<>?"

cellByMousePosition :: Int -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
cellByMousePosition cellSize (mx,my) (r,c)
  | mx < 0 || my < 0 || mx >= cellSize * c || my >= cellSize * r = Nothing
  | otherwise = Just (mx `div` cellSize, my `div` cellSize)

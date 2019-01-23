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
  , rd0, rd1, rd2
  , or0, or1, or2
  , yw0, yw1, yw2
  , ch0, ch1, ch2
  , gn0, gn1, gn2
  , sp0, sp1, sp2
  , cn0, cn1, cn2
  , az0, az1, az2
  , bl0, bl1, bl2
  , vt0, vt1, vt2
  , mg0, mg1, mg2
  , rs0, rs1, rs2
  , br0, br1, br2
  , gy0, gy1, gy2
  , wht, blk
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
import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))
import Control.Applicative ((<|>))

import qualified SDL
import qualified SDL.Primitive as Gfx
import qualified Data.Vector.Storable as VS

data Color
  = Red0
  | Red1
  | Red2
  | Orange0
  | Orange1
  | Orange2
  | Yellow0
  | Yellow1
  | Yellow2
  | Chartreuse0
  | Chartreuse1
  | Chartreuse2
  | Green0
  | Green1
  | Green2
  | Spring0
  | Spring1
  | Spring2
  | Cyan0
  | Cyan1
  | Cyan2
  | Azure0
  | Azure1
  | Azure2
  | Blue0
  | Blue1
  | Blue2
  | Violet0
  | Violet1
  | Violet2
  | Magenta0
  | Magenta1
  | Magenta2
  | Rose0
  | Rose1
  | Rose2
  | Brown0
  | Brown1
  | Brown2
  | Gray0
  | Gray1
  | Gray2
  | White
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
  | Dash
  | Bar
  | Cross
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
  , fill :: Maybe Color
  } deriving (Show, Eq, Generic)

instance ToJSON Cell
instance FromJSON Cell

instance Semigroup Cell where
  (<>) (Cell aShape aFill) (Cell bShape bFill) = case bFill of
    Nothing -> Cell (bShape <|> aShape) aFill
    Just _ -> Cell bShape bFill

instance Monoid Cell where
  mempty = Cell Nothing Nothing

data GridProto s = GridProto
  { title :: String
  , rows :: Int
  , cols :: Int
  , cellPixelSize :: Int
  , backgroundColor :: Color
  , setupFn :: IO s
  , updateFn :: Input -> s -> IO s
  , cleanupFn :: s -> IO ()
  , cellsFn :: s -> Map (Int, Int) Cell
  , quitFn :: s -> Bool
  }

num :: (Integral a, Num b) => a -> b
num = fromIntegral

runGridProto :: GridProto s -> IO ()
runGridProto GridProto
  { title
  , rows
  , cols
  , cellPixelSize
  , backgroundColor
  , setupFn
  , updateFn
  , cellsFn
  , quitFn
  }
  = do
  SDL.initialize [SDL.InitVideo, SDL.InitAudio]
  window <- SDL.createWindow (pack title) SDL.defaultWindow { SDL.windowInitialSize = V2 (num $ rows * cellPixelSize) (num $ cols * cellPixelSize) }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  initialState <- setupFn
  ($ initialState) $ fix $ \loop state -> do
    let quit = quitFn state
    events <- SDL.pollEvents
    (SDL.P mousePos) <- SDL.getAbsoluteMouseLocation
    let (V2 mouseX mouseY) = num <$> mousePos
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

drawFill :: SDL.Renderer -> Int -> (Int, Int) -> Maybe Color -> IO ()
drawFill _ _ _ Nothing = return ()
drawFill renderer cellSize (x,y) (Just color) = do
  let fx0 = x * cellSize
      fx1 = (x + 1) * cellSize
      fy0 = y * cellSize
      fy1 = (y + 1) * cellSize
  Gfx.fillRectangle
    renderer
    (V2 (num fx0) (num fy0))
    (V2 (num fx1) (num fy1))
    (colorPixel color)

drawShape :: SDL.Renderer -> Int -> (Int,  Int) -> (Shape, Color) -> IO ()
drawShape renderer cellSize (x,y) (shape,color) = case shape of
  --
  Circle -> Gfx.smoothCircle renderer center radius color'
  --
  FillCircle -> Gfx.fillCircle renderer center radius color'
  --
  Triangle -> do
    let (dax, day) = triDA
        (dbx, dby) = triDB
        (dcx, dcy) = triDC
        ax = x * cellSize + dax
        ay = y * cellSize + day
        bx = x * cellSize + dbx
        by = y * cellSize + dby
        cx = x * cellSize + dcx
        cy = y * cellSize + dcy
        a = num <$> V2 ax ay
        b = num <$> V2 bx by
        c = num <$> V2 cx cy
    Gfx.thickLine renderer a b thickness' color'
    Gfx.thickLine renderer b c thickness' color'
    Gfx.thickLine renderer c a thickness' color'
  --
  FillTriangle -> do
    let (dax, day) = triDA
        (dbx, dby) = triDB
        (dcx, dcy) = triDC
        ax = x * cellSize + dax
        ay = y * cellSize + day
        bx = x * cellSize + dbx
        by = y * cellSize + dby
        cx = x * cellSize + dcx
        cy = y * cellSize + dcy
    Gfx.fillTriangle
      renderer
      (V2 (num ax) (num ay))
      (V2 (num bx) (num by))
      (V2 (num cx) (num cy))
      (colorPixel color)
  --
  Square -> do
    let fx0 = x * cellSize + thickness
        fx1 = (x + 1) * cellSize - thickness
        fy0 = y * cellSize + thickness
        fy1 = (y + 1) * cellSize - thickness
    Gfx.rectangle
      renderer
      (V2 (num fx0) (num fy0))
      (V2 (num fx1) (num fy1))
      (colorPixel color)
  --
  FillSquare -> do
    let fx0 = x * cellSize + thickness
        fx1 = (x + 1) * cellSize - thickness
        fy0 = y * cellSize + thickness
        fy1 = (y + 1) * cellSize - thickness
    Gfx.fillRectangle
      renderer
      (V2 (num fx0) (num fy0))
      (V2 (num fx1) (num fy1))
      (colorPixel color)
  --
  Plus -> do
    let x' = x * cellSize + halfCell'
        y' = y * cellSize + halfCell'
        a = num <$> V2 x' (y * cellSize + thickness')
        b = num <$> V2 x' ((y + 1) * cellSize - thickness')
        c = num <$> V2 (x * cellSize + thickness') y'
        d = num <$> V2 ((x + 1) * cellSize - thickness') y'
    Gfx.thickLine renderer a b thickness' color'
    Gfx.thickLine renderer c d thickness' color'
  --
  Dash -> do
    let y' = y * cellSize + halfCell'
        a = num <$> V2 (x * cellSize + thickness') y'
        b = num <$> V2 ((x + 1) * cellSize - thickness') y'
    Gfx.thickLine renderer a b thickness' color'
  Bar -> do
    let x' = x * cellSize + halfCell'
        a = num <$> V2 x' (y * cellSize + thickness')
        b = num <$> V2 x' ((y + 1) * cellSize - thickness')
    Gfx.thickLine renderer a b thickness' color'
 --
  Cross -> do
    let diff = halfCell' - thickness
        left = x * cellSize + halfCell' - diff
        right = x * cellSize + halfCell' + diff
        top = y * cellSize + halfCell' - diff
        bottom = y * cellSize + halfCell' + diff
        a = num <$> V2 left top
        b = num <$> V2 right bottom
        c = num <$> V2 right top
        d = num <$> V2 left bottom
    Gfx.thickLine renderer a b thickness' color'
    Gfx.thickLine renderer c d thickness' color'

  where
    thickness' :: Num a => a
    thickness' = num thickness
    thickness :: Int
    thickness = max (cellSize `div` 8) 1
    triAAngle = pi / 2
    triBAngle = 2 * pi / 3 + pi / 2
    triCAngle = 2 * 2 * pi / 3 + pi / 2
    halfCell = fromIntegral cellSize / 2
    halfCell' = cellSize `div` 2
    triCorner angle =
      ( floor $ (halfCell * cos angle) + halfCell
      , floor $ negate (halfCell * sin angle) + halfCell + fromIntegral cellSize * 0.1
      )
    triDA = triCorner triAAngle
    triDB = triCorner triBAngle
    triDC = triCorner triCAngle
    center = (\n -> floor (num (n * cellSize) + halfCell)) <$> V2 x y
    radius = floor $ halfCell * 0.8
    color' = colorPixel color

colorPixel :: Color -> Gfx.Color
colorPixel c = bgr (colorValue c)

bgr :: (Word8, Word8, Word8) -> Gfx.Color
bgr (r,g,b) = V4 (num r) (num g) (num b) 0xff

colorValue :: Integral a => Color -> (a, a, a)
colorValue Red0        = (0xff, 0x44, 0x44)
colorValue Red1        = (0xff, 0x00, 0x00)
colorValue Red2        = (0xaa, 0x00, 0x00)
colorValue Orange0     = (0xff, 0xaf, 0x33)
colorValue Orange1     = (0xff, 0x7f, 0x00)
colorValue Orange2     = (0xcf, 0x4f, 0x00)
colorValue Yellow0     = (0xff, 0xff, 0x44)
colorValue Yellow1     = (0xff, 0xff, 0x00)
colorValue Yellow2     = (0xaa, 0xaa, 0x00)
colorValue Chartreuse0 = (0x00, 0xff, 0x00)
colorValue Chartreuse1 = (0x44, 0xff, 0x44)
colorValue Chartreuse2 = (0x00, 0xaa, 0x00)
colorValue Green0      = (0x44, 0xff, 0x44)
colorValue Green1      = (0x00, 0xff, 0x00)
colorValue Green2      = (0x00, 0xaa, 0x00)
colorValue Spring0     = (0x44, 0xff, 0x44)
colorValue Spring1     = (0x00, 0xff, 0x00)
colorValue Spring2     = (0x00, 0xaa, 0x00)
colorValue Cyan0       = (0x44, 0xff, 0xff)
colorValue Cyan1       = (0x00, 0xff, 0xff)
colorValue Cyan2       = (0x00, 0xaa, 0xaa)
colorValue Azure0      = (0x44, 0x44, 0xff)
colorValue Azure1      = (0x00, 0x00, 0xff)
colorValue Azure2      = (0x00, 0x00, 0xaa)
colorValue Blue0       = (0x44, 0x44, 0xff)
colorValue Blue1       = (0x00, 0x00, 0xff)
colorValue Blue2       = (0x00, 0x00, 0xaa)
colorValue Violet0     = (0x44, 0x44, 0xff)
colorValue Violet1     = (0x00, 0x00, 0xff)
colorValue Violet2     = (0x00, 0x00, 0xaa)
colorValue Magenta0    = (0xff, 0x44, 0xff)
colorValue Magenta1    = (0xff, 0x00, 0xff)
colorValue Magenta2    = (0xaa, 0x00, 0xaa)
colorValue Rose0       = (0xff, 0x99, 0xcc)
colorValue Rose1       = (0xff, 0x66, 0x99)
colorValue Rose2       = (0xaa, 0x22, 0x44)
colorValue Brown0      = (0xaa, 0x77, 0x44)
colorValue Brown1      = (0x88, 0x44, 0x00)
colorValue Brown2      = (0x55, 0x22, 0x00)
colorValue Gray0       = (0xaa, 0xaa, 0xaa)
colorValue Gray1       = (0x77, 0x77, 0x77)
colorValue Gray2       = (0x44, 0x44, 0x44)
colorValue White       = (0xff, 0xff, 0xff)
colorValue Black       = (0x00, 0x00, 0x00)

rd0, rd1, rd2,
  or0, or1, or2,
  yw0, yw1, yw2,
  ch0, ch1, ch2,
  gn0, gn1, gn2,
  sp0, sp1, sp2,
  cn0, cn1, cn2,
  az0, az1, az2,
  bl0, bl1, bl2,
  vt0, vt1, vt2,
  mg0, mg1, mg2,
  rs0, rs1, rs2,
  br0, br1, br2,
  gy0, gy1, gy2,
  wht, blk :: Color
(rd0, rd1, rd2) = (Red0, Red1, Red2)
(or0, or1, or2) = (Orange0, Orange1, Orange2)
(yw0, yw1, yw2) = (Yellow0, Yellow1, Yellow2)
(ch0, ch1, ch2) = (Chartreuse0, Chartreuse1, Chartreuse2)
(gn0, gn1, gn2) = (Green0, Green1, Green2)
(sp0, sp1, sp2) = (Spring0, Spring1, Spring2)
(cn0, cn1, cn2) = (Cyan0, Cyan1, Cyan2)
(az0, az1, az2) = (Azure0, Azure1, Azure2)
(bl0, bl1, bl2) = (Blue0, Blue1, Blue2)
(vt0, vt1, vt2) = (Violet0, Violet1, Violet2)
(mg0, mg1, mg2) = (Magenta0, Magenta1, Magenta2)
(rs0, rs1, rs2) = (Rose0, Rose1, Rose2)
(br0, br1, br2) = (Brown0, Brown1, Brown2)
(gy0, gy1, gy2) = (Gray0, Gray1, Gray2)
(wht, blk)      = (White, Black)

rainbow :: [Color]
rainbow = [rd1, or1, yw1, gn1, sp1, cn1, az1, bl1, vt1, mg1, rs1]

-- symbolList :: [Char]
-- symbolList = "`1234567890-=~!@#$%^&*()_+qwertyuiop[]\\QWERTYUIOP{}|asdfghjkl;'ASDFGHJKL:\"zxcvbnm,./ZXCVBNM<>?"

cellByMousePosition :: Int -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
cellByMousePosition cellSize (mx,my) (r,c)
  | mx < 0 || my < 0 || mx >= cellSize * c || my >= cellSize * r = Nothing
  | otherwise = Just (mx `div` cellSize, my `div` cellSize)

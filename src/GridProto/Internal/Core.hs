{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-} -- instance MapTile View
{-# LANGUAGE FlexibleInstances #-} -- instance MapTile View
module GridProto.Internal.Core where

import Prelude hiding (lookup)
import GHC.Generics (Generic)
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (when)
import Data.Traversable (forM)
import Data.Function (fix)
import Data.Foldable (forM_)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Data.Map (Map, fromList, (!), delete, alter, insert, filterWithKey, member, notMember, toList)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.Text (pack)
import Data.Word (Word8, Word32)
import Data.Int (Int16)
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import SDL.Input.Keyboard.Codes
import SDL.Input.GameController (ControllerButton(..), ControllerButtonState(..), ControllerDeviceConnection(..))
import GridProto.Internal.Font

import qualified Data.Map as Map
import qualified Data.List as L
import qualified Data.Vector.Storable as VS
import qualified SDL
import qualified SDL.Raw.Event as Raw
import qualified SDL.Font as Font
import qualified SDL.Primitive as Gfx
import qualified SDL.Mixer as Mixer

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
  | White0
  | White1
  | White2
  | Black0
  | Black1
  | Black2
  deriving (Enum, Eq, Bounded, Ord, Show, Generic)

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

data Mouse = Mouse
  { mousePosition :: (Int, Int)
  , mouseButton :: KeyState
  } deriving (Show, Eq, Generic)

data Key
  = Char Char
  | UpArrow
  | DownArrow
  | LeftArrow
  | RightArrow
  | Enter
  | Escape
  | LeftShift
  | RightShift
  | LeftControl
  | RightControl
  | LeftAlt
  | RightAlt
  | Tab
  | Backspace
  | Meta
  deriving (Eq, Show, Ord, Generic)

data KeyState
  = Pressed
  | Held
  | Released
  | Untouched
  deriving (Enum, Eq, Bounded, Show, Generic)

newtype Keys = Keys { unKeys :: Map Key KeyState }
  deriving (Show, Eq, Generic)

data Axis = Axis
  { xAxis :: Float
  , yAxis :: Float
  } deriving (Show, Eq, Generic)

data Controller = Controller
  { isConnected :: Bool
  , startButton :: KeyState
  , backButton :: KeyState
  , dpadUp :: KeyState
  , dpadDown :: KeyState
  , dpadLeft :: KeyState
  , dpadRight :: KeyState
  , aButton :: KeyState
  , bButton :: KeyState
  , xButton :: KeyState
  , yButton :: KeyState
  , leftStick :: KeyState
  , rightStick :: KeyState
  , leftShoulder :: KeyState
  , rightShoulder :: KeyState
  , leftAxis :: Axis
  , rightAxis :: Axis
  } deriving (Show, Eq, Generic)

initController :: Controller
initController = Controller
  False
  Untouched
  Untouched
  Untouched
  Untouched
  Untouched
  Untouched
  Untouched
  Untouched
  Untouched
  Untouched
  Untouched
  Untouched
  Untouched
  Untouched
  (Axis 0 0)
  (Axis 0 0)

data Input = Input
  { mouse :: Mouse
  , keys :: Keys
  , controller1 :: Controller
  , controller2 :: Controller
  , controller3 :: Controller
  , controller4 :: Controller
  } deriving (Show, Eq, Generic)

data Tile = Tile
  { symbol :: Maybe (Char, Color)
  , shape :: Maybe (Shape, Color)
  , fill :: Maybe Color
  } deriving (Show, Eq, Generic)

instance Semigroup Tile where
  (<>) (Tile aSymbol aShape aFill) (Tile bSymbol bShape bFill) = case bFill of
    Nothing -> Tile (bSymbol <|> aSymbol) (bShape <|> aShape) aFill
    Just _ -> Tile bSymbol bShape bFill

instance Monoid Tile where
  mempty = Tile Nothing Nothing Nothing

type View = Map (Int, Int) Tile


data Viewport = Viewport
  { vpView :: View
  , vpXY :: (Int, Int)
  , vpDim :: (Int, Int)
  } deriving (Show, Eq, Generic)

class MapTile a where
  mapTile
    :: ((Char, Color) -> (Char, Color))
    -> ((Shape, Color) -> (Shape, Color))
    -> (Color -> Color)
    -> a
    -> a
  --
  mapSymbol :: ((Char, Color) -> (Char, Color)) -> a -> a
  mapSymbol f = mapTile f id id
  --
  mapShape :: ((Shape, Color) -> (Shape, Color)) -> a -> a
  mapShape f = mapTile id f id
  --
  mapFill :: (Color -> Color) -> a -> a
  mapFill f = mapTile id id f

instance MapTile Tile where
  mapTile symbolFn shapeFn fillFn tile = Tile
    (symbolFn <$> symbol tile)
    (shapeFn <$> shape tile)
    (fillFn <$> fill tile)

emptyView :: View
emptyView = Map.fromList []

instance MapTile View where
  mapTile symbolFn shapeFn fillFn = fmap (mapTile symbolFn shapeFn fillFn)

instance MapTile Viewport where
  mapTile symbolFn shapeFn fillFn vp = vp { vpView = mapTile symbolFn shapeFn fillFn (vpView vp) }

lookupMap :: Ord k => k -> Map k a -> Maybe a
lookupMap = Map.lookup

num :: (Integral a, Num b) => a -> b
num = fromIntegral

lookupKey :: Keys -> Key -> KeyState
lookupKey (Keys m) k = fromMaybe Untouched (Map.lookup k m)

makeInput :: Input -> Maybe (Int, Int) -> Bool -> [SDL.EventPayload] -> Input
makeInput Input{mouse,keys,controller1,controller2,controller3,controller4} mpos' mclick eventPayloads = Input m (Keys $ nextKeys $ unKeys keys) controller1' controller2' controller3' controller4'
  where
    mpos = fromMaybe (mousePosition mouse) mpos'
    mbutton
      | mclick && mouseButton mouse == Untouched = Pressed
      | mclick && mouseButton mouse == Pressed = Held
      | mclick && mouseButton mouse == Held = Held
      | not mclick && mouseButton mouse == Held = Released
      | otherwise = Untouched
    m = Mouse mpos mbutton
    keyChanges = Map.fromList . catMaybes $ map keyChange eventPayloads
    removeReleased = Map.filter (/= Released)
    pressedToHeld = Map.map stepKeyState
    nextKeys = Map.union keyChanges . pressedToHeld . removeReleased
    controller1' = foldr (applyControllerChange 0) (stepController controller1) eventPayloads
    controller2' = foldr (applyControllerChange 1) (stepController controller2) eventPayloads
    controller3' = foldr (applyControllerChange 2) (stepController controller3) eventPayloads
    controller4' = foldr (applyControllerChange 3) (stepController controller4) eventPayloads

normalizeInt16 :: Int16 -> Float
normalizeInt16 w = let
  f = fromIntegral w / (fromIntegral (maxBound :: Int16))
  deadzone x = if x < 0.05 && x > -0.05 then 0 else x
  clamp x = if x > 1 then 1 else (if x < -1 then -1 else x)
  in clamp $ deadzone f

applyControllerChange :: Int -> SDL.EventPayload -> Controller -> Controller
applyControllerChange idx event c = case event of
  SDL.ControllerDeviceEvent (SDL.ControllerDeviceEventData ControllerDeviceRemoved j) -> if j == jId then c { isConnected = False } else c
  SDL.ControllerButtonEvent (SDL.ControllerButtonEventData 0 button buttonState) -> fromMaybe c (update button <$> toKeyState buttonState)
  SDL.ControllerAxisEvent (SDL.ControllerAxisEventData j 0 i) -> if j == jId then c { leftAxis = (leftAxis c) { xAxis = normalizeInt16 i } } else c
  SDL.ControllerAxisEvent (SDL.ControllerAxisEventData j 1 i) -> if j == jId then c { leftAxis = (leftAxis c) { yAxis = normalizeInt16 i } } else c
  SDL.ControllerAxisEvent (SDL.ControllerAxisEventData j 2 i) -> if j == jId then c { rightAxis = (rightAxis c) { xAxis = normalizeInt16 i } } else c
  SDL.ControllerAxisEvent (SDL.ControllerAxisEventData j 3 i) -> if j == jId then c { rightAxis = (rightAxis c) { yAxis = normalizeInt16 i } } else c
  --
  SDL.JoyAxisEvent (SDL.JoyAxisEventData j 0 i) -> if j == jId then c { leftAxis = (leftAxis c) { xAxis = normalizeInt16 i } } else c
  SDL.JoyAxisEvent (SDL.JoyAxisEventData j 1 i) -> if j == jId then c { leftAxis = (leftAxis c) { yAxis = normalizeInt16 i } } else c
  SDL.JoyAxisEvent (SDL.JoyAxisEventData j 2 i) -> if j == jId then c { rightAxis = (rightAxis c) { xAxis = normalizeInt16 i } } else c
  SDL.JoyAxisEvent (SDL.JoyAxisEventData j 3 i) -> if j == jId then c { rightAxis = (rightAxis c) { yAxis = normalizeInt16 i } } else c
  _ -> c
  where
    toKeyState buttonState = case buttonState of
      ControllerButtonPressed -> Just Pressed
      ControllerButtonReleased -> Just Released
      _ -> Nothing
    update button v = case button of
      ControllerButtonStart -> c { startButton = v }
      ControllerButtonBack -> c { backButton = v }
      ControllerButtonDpadUp -> c { dpadUp = v }
      ControllerButtonDpadDown -> c { dpadDown = v }
      ControllerButtonDpadLeft -> c { dpadLeft = v }
      ControllerButtonDpadRight -> c { dpadRight = v }
      ControllerButtonA -> c { aButton = v }
      ControllerButtonB -> c { bButton = v }
      ControllerButtonX -> c { xButton = v }
      ControllerButtonY -> c { yButton = v }
      ControllerButtonLeftStick -> c { leftStick = v }
      ControllerButtonRightStick -> c { rightStick = v }
      ControllerButtonLeftShoulder -> c { leftShoulder = v }
      ControllerButtonRightShoulder -> c { rightShoulder = v }
      _ -> c
    jId = fromIntegral idx

stepController :: Controller -> Controller
stepController c = c
  { startButton = stepKeyState $ startButton c
  , backButton = stepKeyState $ backButton c
  , dpadUp = stepKeyState $ dpadUp c
  , dpadDown = stepKeyState $ dpadDown c
  , dpadLeft = stepKeyState $ dpadLeft c
  , dpadRight = stepKeyState $ dpadRight c
  , aButton = stepKeyState $ aButton c
  , bButton = stepKeyState $ bButton c
  , xButton = stepKeyState $ xButton c
  , yButton = stepKeyState $ yButton c
  , leftStick = stepKeyState $ leftStick c
  , rightStick = stepKeyState $ rightStick c
  , leftShoulder = stepKeyState $ leftShoulder c
  , rightShoulder = stepKeyState $ rightShoulder c
  }

stepKeyState :: KeyState -> KeyState
stepKeyState ks = case ks of
  Pressed -> Held
  Held -> Held
  Released -> Untouched
  Untouched -> Untouched

keyFromKeyCode :: SDL.Keycode -> Maybe Key
keyFromKeyCode = \case
  KeycodeLeft -> Just LeftArrow
  KeycodeDown -> Just DownArrow
  KeycodeUp -> Just UpArrow
  KeycodeRight -> Just RightArrow
  KeycodeReturn -> Just Enter
  KeycodeEscape -> Just Escape
  KeycodeLShift -> Just LeftShift
  KeycodeRShift -> Just RightShift
  KeycodeLCtrl -> Just LeftControl
  KeycodeRCtrl -> Just RightControl
  KeycodeLAlt -> Just LeftAlt
  KeycodeRAlt -> Just RightAlt
  KeycodeTab -> Just Tab
  KeycodeBackspace -> Just Backspace
  KeycodeLGUI -> Just Meta
  KeycodeRGUI -> Just Meta
  --
  KeycodeA -> Just $ Char 'a'
  KeycodeB -> Just $ Char 'b'
  KeycodeC -> Just $ Char 'c'
  KeycodeD -> Just $ Char 'd'
  KeycodeE -> Just $ Char 'e'
  KeycodeF -> Just $ Char 'f'
  KeycodeG -> Just $ Char 'g'
  KeycodeH -> Just $ Char 'h'
  KeycodeI -> Just $ Char 'i'
  KeycodeJ -> Just $ Char 'j'
  KeycodeK -> Just $ Char 'k'
  KeycodeL -> Just $ Char 'l'
  KeycodeM -> Just $ Char 'm'
  KeycodeN -> Just $ Char 'n'
  KeycodeO -> Just $ Char 'o'
  KeycodeP -> Just $ Char 'p'
  KeycodeQ -> Just $ Char 'q'
  KeycodeR -> Just $ Char 'r'
  KeycodeS -> Just $ Char 's'
  KeycodeT -> Just $ Char 't'
  KeycodeU -> Just $ Char 'u'
  KeycodeV -> Just $ Char 'v'
  KeycodeW -> Just $ Char 'w'
  KeycodeX -> Just $ Char 'x'
  KeycodeY -> Just $ Char 'y'
  KeycodeZ -> Just $ Char 'z'
  --
  Keycode0 -> Just $ Char '0'
  Keycode1 -> Just $ Char '1'
  Keycode2 -> Just $ Char '2'
  Keycode3 -> Just $ Char '3'
  Keycode4 -> Just $ Char '4'
  Keycode5 -> Just $ Char '5'
  Keycode6 -> Just $ Char '6'
  Keycode7 -> Just $ Char '7'
  Keycode8 -> Just $ Char '8'
  Keycode9 -> Just $ Char '9'
  --
  KeycodeBackquote -> Just $ Char '`'
  KeycodeMinus -> Just $ Char '-'
  KeycodeEquals -> Just $ Char '='
  KeycodeLeftBracket -> Just $ Char '['
  KeycodeRightBracket -> Just $ Char ']'
  KeycodeBackslash -> Just $ Char '\\'
  KeycodeSemicolon -> Just $ Char ';'
  KeycodeQuote -> Just $ Char '\''
  KeycodeComma -> Just $ Char ','
  KeycodePeriod -> Just $ Char '.'
  KeycodeSlash -> Just $ Char '/'
  KeycodeSpace -> Just $ Char ' '
  --
  _ -> Nothing

keyChange :: SDL.EventPayload -> Maybe (Key, KeyState)
keyChange event = case event of
    SDL.KeyboardEvent SDL.KeyboardEventData{SDL.keyboardEventKeysym = SDL.Keysym{SDL.keysymKeycode = code}, SDL.keyboardEventKeyMotion = motion, SDL.keyboardEventRepeat } -> if not keyboardEventRepeat
        then case motion of
          SDL.Released ->  (\k -> (k, Released)) <$> keyFromKeyCode code
          SDL.Pressed -> (\k -> (k, Pressed)) <$> keyFromKeyCode code
        else Nothing
    _ -> Nothing


drawTileMap :: Color -> SDL.Renderer -> Int -> (Color -> Char -> IO (Maybe (SDL.Texture, Int, Int))) -> View -> IO ()
drawTileMap bgColor renderer tileSize fontMap m = forM_ (toList m) $ \((x,y), Tile{symbol,shape,fill}) -> do
  drawFill renderer tileSize (x,y) fill
  case shape of
    Nothing -> return ()
    Just shape' -> drawShape (fromMaybe bgColor fill) renderer tileSize (x,y) shape'
  case symbol of
    Nothing -> return ()
    Just (symbol', color) -> drawSymbol renderer fontMap symbol' color tileSize (x,y)


drawFill :: SDL.Renderer -> Int -> (Int, Int) -> Maybe Color -> IO ()
drawFill _ _ _ Nothing = return ()
drawFill renderer tileSize (x,y) (Just color) = do
  let fx0 = x * tileSize
      fx1 = (x + 1) * tileSize
      fy0 = y * tileSize
      fy1 = (y + 1) * tileSize
  Gfx.fillRectangle
    renderer
    (V2 (num fx0) (num fy0))
    (V2 (num fx1) (num fy1))
    (colorPixel color)

drawShape :: Color -> SDL.Renderer -> Int -> (Int,  Int) -> (Shape, Color) -> IO ()
drawShape bgColor renderer tileSize (x,y) (shape,color) = case shape of
  --
  Circle -> do
    Gfx.fillCircle renderer center radius color'
    Gfx.fillCircle renderer center (radius - thickness') (colorPixel bgColor)
  --
  FillCircle -> Gfx.fillCircle renderer center radius color'
  --
  Triangle -> do
    let drawTri a b c co = do
          let (dax, day) = a
              (dbx, dby) = b
              (dcx, dcy) = c
              ax = x * tileSize + dax
              ay = y * tileSize + day
              bx = x * tileSize + dbx
              by = y * tileSize + dby
              cx = x * tileSize + dcx
              cy = y * tileSize + dcy
          Gfx.fillTriangle
            renderer
            (V2 (num ax) (num ay))
            (V2 (num bx) (num by))
            (V2 (num cx) (num cy))
            (colorPixel co)
    drawTri triDA  triDB  triDC  color
    drawTri triDA' triDB' triDC' bgColor
  --
  FillTriangle -> do
    let (dax, day) = triDA
        (dbx, dby) = triDB
        (dcx, dcy) = triDC
        ax = x * tileSize + dax
        ay = y * tileSize + day
        bx = x * tileSize + dbx
        by = y * tileSize + dby
        cx = x * tileSize + dcx
        cy = y * tileSize + dcy
    Gfx.fillTriangle
      renderer
      (V2 (num ax) (num ay))
      (V2 (num bx) (num by))
      (V2 (num cx) (num cy))
      (colorPixel color)
  --
  Square -> do
    let drawSquare thk co = do
          let fx0 = x * tileSize + thk
              fx1 = (x + 1) * tileSize - thk
              fy0 = y * tileSize + thk
              fy1 = (y + 1) * tileSize - thk
          Gfx.fillRectangle
            renderer
            (V2 (num fx0) (num fy0))
            (V2 (num fx1) (num fy1))
            (colorPixel co)
    drawSquare thickness color
    drawSquare (thickness * 2) bgColor
  --
  FillSquare -> do
    let fx0 = x * tileSize + thickness
        fx1 = (x + 1) * tileSize - thickness
        fy0 = y * tileSize + thickness
        fy1 = (y + 1) * tileSize - thickness
    Gfx.fillRectangle
      renderer
      (V2 (num fx0) (num fy0))
      (V2 (num fx1) (num fy1))
      (colorPixel color)
  --
  Plus -> do
    let x' = x * tileSize + halfTile'
        y' = y * tileSize + halfTile'
        a = num <$> V2 x' (y * tileSize + thickness')
        b = num <$> V2 x' ((y + 1) * tileSize - thickness')
        c = num <$> V2 (x * tileSize + thickness') y'
        d = num <$> V2 ((x + 1) * tileSize - thickness') y'
    Gfx.thickLine renderer a b thickness' color'
    Gfx.thickLine renderer c d thickness' color'
  --
  Dash -> do
    let y' = y * tileSize + halfTile'
        a = num <$> V2 (x * tileSize + thickness') y'
        b = num <$> V2 ((x + 1) * tileSize - thickness') y'
    Gfx.thickLine renderer a b thickness' color'
  Bar -> do
    let x' = x * tileSize + halfTile'
        a = num <$> V2 x' (y * tileSize + thickness')
        b = num <$> V2 x' ((y + 1) * tileSize - thickness')
    Gfx.thickLine renderer a b thickness' color'
 --
  Cross -> do
    let diff = halfTile' - thickness
        left = x * tileSize + halfTile' - diff
        right = x * tileSize + halfTile' + diff
        top = y * tileSize + halfTile' - diff
        bottom = y * tileSize + halfTile' + diff
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
    thickness = max (tileSize `div` 12) 1
    --
    triAAngle = pi / 2
    triBAngle = 2 * pi / 3 + pi / 2
    triCAngle = 2 * 2 * pi / 3 + pi / 2
    halfTile = fromIntegral tileSize / 2
    halfTile' = tileSize `div` 2
    triCorner angle radius =
      ( floor $ (radius * cos angle) + halfTile
      , floor $ negate (radius * sin angle) + halfTile + fromIntegral tileSize * 0.1
      )
    --
    triDA  = triCorner triAAngle halfTile
    triDB  = triCorner triBAngle halfTile
    triDC  = triCorner triCAngle halfTile
    --
    innerHalf = halfTile - fromIntegral thickness * 2
    triDA' = triCorner triAAngle innerHalf
    triDB' = triCorner triBAngle innerHalf
    triDC' = triCorner triCAngle innerHalf
    --
    center = (\n -> floor (num (n * tileSize) + halfTile)) <$> V2 x y
    radius = floor $ halfTile * 0.8
    color' = colorPixel color

drawSymbol :: SDL.Renderer -> (Color -> Char -> IO (Maybe (SDL.Texture, Int, Int))) -> Char -> Color -> Int -> (Int, Int) -> IO ()
drawSymbol renderer fontMap ch color tileSize (x,y) = do
  m <- fontMap color ch
  case m of
    Nothing -> return ()
    Just (tex, offsetX, offsetWidth) -> do
      SDL.TextureInfo{SDL.textureWidth=_texWidth,SDL.textureHeight=texHeight} <- SDL.queryTexture tex
      let wh = V2 (fromIntegral offsetWidth) texHeight
      let wh2 = V2 (div (fromIntegral offsetWidth) 2) (div texHeight 2)
      let xy' = xy + center - wh2
      SDL.copy
        renderer
        tex
        (Just $ SDL.Rectangle (SDL.P (fromIntegral <$> V2 offsetX 0)) (V2 (fromIntegral offsetWidth) texHeight))
        (Just $ SDL.Rectangle (SDL.P xy') wh)
  where
    xy = fromIntegral <$> V2 (tileSize * x) (tileSize * y)
    center = fromIntegral <$> V2 (tileSize `div` 2) (tileSize `div` 2)

colorPixel :: Color -> Gfx.Color
colorPixel c = bgr (colorValue c)

sdlColor :: Color -> Gfx.Color
sdlColor = bgr . colorValue

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
colorValue Chartreuse0 = (0xb2, 0xff, 0x66)
colorValue Chartreuse1 = (0x7f, 0xff, 0x00)
colorValue Chartreuse2 = (0x58, 0xb2, 0x00)
colorValue Green0      = (0x44, 0xff, 0x44)
colorValue Green1      = (0x00, 0xff, 0x00)
colorValue Green2      = (0x00, 0xaa, 0x00)
colorValue Spring0     = (0x66, 0xff, 0xb2)
colorValue Spring1     = (0x00, 0xff, 0x7f)
colorValue Spring2     = (0x00, 0x99, 0x4c)
colorValue Cyan0       = (0xa0, 0xff, 0xff)
colorValue Cyan1       = (0x00, 0xff, 0xff)
colorValue Cyan2       = (0x00, 0x8b, 0x8b)
colorValue Azure0      = (0x00, 0x7f, 0xff)
colorValue Azure1      = (0x33, 0x66, 0x99)
colorValue Azure2      = (0x00, 0x33, 0x66)
colorValue Blue0       = (0x44, 0x44, 0xff)
colorValue Blue1       = (0x00, 0x00, 0xff)
colorValue Blue2       = (0x00, 0x00, 0xaa)
colorValue Violet0     = (0x93, 0x70, 0xdb)
colorValue Violet1     = (0x94, 0x00, 0xd3)
colorValue Violet2     = (0x80, 0x00, 0x80)
colorValue Magenta0    = (0xff, 0x9e, 0xff)
colorValue Magenta1    = (0xff, 0x00, 0xff)
colorValue Magenta2    = (0x8b, 0x00, 0x8b)
colorValue Rose0       = (0xff, 0x99, 0xcc)
colorValue Rose1       = (0xff, 0x66, 0x99)
colorValue Rose2       = (0xaa, 0x22, 0x44)
colorValue Brown0      = (0xaa, 0x77, 0x44)
colorValue Brown1      = (0x88, 0x44, 0x00)
colorValue Brown2      = (0x55, 0x22, 0x00)
colorValue Gray0       = (0xd3, 0xd3, 0xd3)
colorValue Gray1       = (0x80, 0x80, 0x80)
colorValue Gray2       = (0xa9, 0xa9, 0xa9)
colorValue White0      = (0xff, 0xff, 0xf0)
colorValue White1      = (0xff, 0xff, 0xff)
colorValue White2      = (0xf0, 0xff, 0xff)
colorValue Black0      = (0x10, 0x00, 0x00)
colorValue Black1      = (0x00, 0x00, 0x00)
colorValue Black2      = (0x00, 0x10, 0x10)

shade :: Color -> Color
shade c = case c of
  Red0 -> Red1
  Red1 -> Red2
  Red2 -> Red2
  Orange0 -> Orange1
  Orange1 -> Orange2
  Orange2 -> Orange2
  Yellow0 -> Yellow1
  Yellow1 -> Yellow2
  Yellow2 -> Yellow2
  Chartreuse0 -> Chartreuse1
  Chartreuse1 -> Chartreuse2
  Chartreuse2 -> Chartreuse2
  Green0 -> Green1
  Green1 -> Green2
  Green2 -> Green2
  Spring0 -> Spring1
  Spring1 -> Spring2
  Spring2 -> Spring2
  Cyan0 -> Cyan1
  Cyan1 -> Cyan2
  Cyan2 -> Cyan2
  Azure0 -> Azure1
  Azure1 -> Azure2
  Azure2 -> Azure2
  Blue0 -> Blue1
  Blue1 -> Blue2
  Blue2 -> Blue2
  Violet0 -> Violet1
  Violet1 -> Violet2
  Violet2 -> Violet2
  Magenta0 -> Magenta1
  Magenta1 -> Magenta2
  Magenta2 -> Magenta2
  Rose0 -> Rose1
  Rose1 -> Rose2
  Rose2 -> Rose2
  Brown0 -> Brown1
  Brown1 -> Brown2
  Brown2 -> Brown2
  Gray0 -> Gray1
  Gray1 -> Gray2
  Gray2 -> Gray2
  White0 -> White1
  White1 -> White2
  White2 -> White2
  Black0 -> Black1
  Black1 -> Black2
  Black2 -> Black2

tint :: Color -> Color
tint c = case c of
  Red0 -> Red0
  Red1 -> Red0
  Red2 -> Red1
  Orange0 -> Orange0
  Orange1 -> Orange0
  Orange2 -> Orange1
  Yellow0 -> Yellow0
  Yellow1 -> Yellow0
  Yellow2 -> Yellow1
  Chartreuse0 -> Chartreuse0
  Chartreuse1 -> Chartreuse0
  Chartreuse2 -> Chartreuse1
  Green0 -> Green0
  Green1 -> Green0
  Green2 -> Green1
  Spring0 -> Spring0
  Spring1 -> Spring0
  Spring2 -> Spring1
  Cyan0 -> Cyan0
  Cyan1 -> Cyan0
  Cyan2 -> Cyan1
  Azure0 -> Azure0
  Azure1 -> Azure0
  Azure2 -> Azure1
  Blue0 -> Blue0
  Blue1 -> Blue0
  Blue2 -> Blue1
  Violet0 -> Violet0
  Violet1 -> Violet0
  Violet2 -> Violet1
  Magenta0 -> Magenta0
  Magenta1 -> Magenta0
  Magenta2 -> Magenta1
  Rose0 -> Rose0
  Rose1 -> Rose0
  Rose2 -> Rose1
  Brown0 -> Brown0
  Brown1 -> Brown0
  Brown2 -> Brown1
  Gray0 -> Gray0
  Gray1 -> Gray0
  Gray2 -> Gray1
  White0 -> White0
  White1 -> White0
  White2 -> White1
  Black0 -> Black0
  Black1 -> Black0
  Black2 -> Black1

rd0, rd1, rd2,
  or0, or1, or2,
  yw0, yw1, yw2,
  ch0, ch1, ch2,
  gn0, gn1, gn2,
  sp0, sp1, sp2,
  cn0, cn1, cn2,
  az0, az1, az2,
  bu0, bu1, bu2,
  vt0, vt1, vt2,
  mg0, mg1, mg2,
  rs0, rs1, rs2,
  br0, br1, br2,
  gy0, gy1, gy2,
  wh0, wh1, wh2,
  bk0, bk1, bk2 :: Color
(rd0, rd1, rd2) = (Red0, Red1, Red2)
(or0, or1, or2) = (Orange0, Orange1, Orange2)
(yw0, yw1, yw2) = (Yellow0, Yellow1, Yellow2)
(ch0, ch1, ch2) = (Chartreuse0, Chartreuse1, Chartreuse2)
(gn0, gn1, gn2) = (Green0, Green1, Green2)
(sp0, sp1, sp2) = (Spring0, Spring1, Spring2)
(cn0, cn1, cn2) = (Cyan0, Cyan1, Cyan2)
(az0, az1, az2) = (Azure0, Azure1, Azure2)
(bu0, bu1, bu2) = (Blue0, Blue1, Blue2)
(vt0, vt1, vt2) = (Violet0, Violet1, Violet2)
(mg0, mg1, mg2) = (Magenta0, Magenta1, Magenta2)
(rs0, rs1, rs2) = (Rose0, Rose1, Rose2)
(br0, br1, br2) = (Brown0, Brown1, Brown2)
(gy0, gy1, gy2) = (Gray0, Gray1, Gray2)
(wh0, wh1, wh2) = (White0, White1, White2)
(bk0, bk1, bk2) = (Black0, Black1, Black2)

rainbow :: [Color]
rainbow = [rd1, or1, yw1, ch1, gn1, sp1, cn1, az1, bu1, vt1, mg1, rs1]

warms :: [Color]
warms = [rd1, or1, yw1, rs1]

cools :: [Color]
cools = [ch1, gn1, sp1, cn1, az1, bu1, vt1, mg1]

tileByMousePosition :: Int -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
tileByMousePosition tileSize (mx,my) (r,c)
  | mx < 0 || my < 0 || mx >= tileSize * c || my >= tileSize * r = Nothing
  | otherwise = Just (mx `div` tileSize, my `div` tileSize)

symbolList :: [Char]
symbolList = "`1234567890-=~!@#$%^&*()_+qwertyuiop[]\\QWERTYUIOP{}|asdfghjkl;'ASDFGHJKL:\"zxcvbnm,./ZXCVBNM<>?"

toTexture :: SDL.Renderer -> SDL.Surface -> IO SDL.Texture
toTexture renderer surface = do
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  return texture

drawTile :: View -> (Int, Int) -> Tile -> View
drawTile m xy tile = Map.insertWith (flip (<>)) xy tile m

drawView
  :: View -- | Base view
  -> (Int, Int)  -- | Offset
  -> View -- | Tiles to be placed
  -> View
drawView old (x,y) new = foldr (\((x',y'), tile) m' -> drawTile m' (x+x', y+y') tile) old (Map.toList new)

mergeView
  :: View -- | Base view
  -> View -- | View to be placed
  -> View
mergeView old new = drawView old (0,0) new

mergeViews
  :: [View] -- | views
  -> View
mergeViews views = foldl mergeView emptyView views

mergeViewport
  :: View -- | Base view
  -> Viewport -- | To be placed
  -> View
mergeViewport old vp = drawView old (vpXY vp) (Map.filterWithKey (\(x,y) _ -> x < w && y < h) (vpView vp))
  where
    (w,h) = vpDim vp

mergeViewports
  :: View -- | Base view
  -> [Viewport] -- | To be placed
  -> View
mergeViewports = L.foldl' mergeViewport

loadFont :: SDL.Renderer -> Int -> IO (Font.Font, Int)
loadFont renderer tileSize = (,) <$> Font.decode fontData size <*> pure size
  where
    size = tileSize `div` 2

newFontColorMap :: IO (IORef (Map Color SDL.Texture))
newFontColorMap = newIORef Map.empty

loadSymbols :: SDL.Renderer -> Font.Font -> Color -> IO SDL.Texture
loadSymbols renderer font color = do
  symSurface <- Font.solid font (colorPixel color) (pack symbolList)
  toTexture renderer symSurface

findSymbols
  :: SDL.Renderer
  -> Font.Font
  -> Int
  -> IORef (Map Color SDL.Texture)
  -> Color
  -> Char
  -> IO (Maybe (SDL.Texture, Int, Int))
findSymbols renderer font width ref color ch = do
  let width' = width `div` 2
  fontMap <- readIORef ref
  case lookupMap color fontMap of
    Just tex -> case lookupMap ch offsets of
      Nothing -> return Nothing
      Just off -> return $ Just (tex, off * width', width')
    Nothing -> case lookupMap ch offsets of
      Nothing -> return Nothing
      Just off -> do
        sym <- loadSymbols renderer font color
        modifyIORef ref (insert color sym)
        return $ Just (sym, off * width', width')
  where
    offsets = Map.fromList $ zip symbolList [0..]

drawString :: Color -> (Int,Int) -> String -> View
drawString color (x,y) str = fromList $ zipWith tile [0..] str
  where
    tile i sym = ((x + i, y), Tile (Just (sym, color)) Nothing Nothing)

colorWheel0 :: [Color]
colorWheel0 = [Red0, Orange0, Yellow0, Chartreuse0, Green0, Spring0, Cyan0, Azure0, Blue0, Violet0, Magenta0, Rose0]

colorWheel1 :: [Color]
colorWheel1 = [Red1, Orange1, Yellow1, Chartreuse1, Green1, Spring1, Cyan1, Azure1, Blue1, Violet1, Magenta1, Rose1]

colorWheel2 :: [Color]
colorWheel2 = [Red2, Orange2, Yellow2, Chartreuse2, Green2, Spring2, Cyan2, Azure2, Blue2, Violet2, Magenta2, Rose2]

class Monad m => FPS m where
  startFrame :: m Word32
  default startFrame :: MonadIO m => m Word32
  startFrame = liftIO SDL.ticks

  endFrame :: Int -> Word32 -> m ()
  default endFrame :: MonadIO m => Int -> Word32 -> m ()
  endFrame = endFrame'

instance FPS IO

-- | `endFrame`'s default definition
endFrame' :: MonadIO m => Int -> Word32 -> m ()
endFrame' fps startTicks = liftIO $ do
  endTicks <- SDL.ticks
  let diff = (endTicks - startTicks) * fps'
  when (msps > diff) $ do
    let ms = (msps - diff) `div` fps'
    SDL.delay (fromIntegral ms)
  where
    fps' = fromIntegral fps

-- | Same as default definition and prints fps and delay
endFrameDebug :: MonadIO m => Int -> Word32 -> m ()
endFrameDebug fps startTicks = liftIO $ do
  endTicks <- SDL.ticks
  let diff = (endTicks - startTicks) * fps'
  let ms = (msps - diff) `div` fps'
  when (msps > diff) $ SDL.delay (fromIntegral ms)
  putStrLn $ show fps ++ " fps - before delay " ++ show (endTicks - startTicks) ++ " ms" ++ " with expected max " ++ show (msps `div` fps') ++ " ms"
  where
    fps' = fromIntegral fps

-- | Milliseconds per second
msps :: Word32
msps = 1000

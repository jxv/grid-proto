# Grid Proto

### Game Engine for Prototyping on a Grid

Quickly prototype a graphical program with basic input, colors, shapes, and text.
Grid Proto follows an 'anti-polished' philosophy. It exposes a heavily limited API over SDL2 which doesn't allow for any user media.
*No images, fonts, or audio can be loaded.*
This usage is one step above prototyping within the terminal. So, just write a function for updating state, a pure function for rendering, and a pure function for sound effects.

---

### Hello World

```haskell
import GridProto

main :: IO ()
main = runGridProto helloWorld ()

helloWorld :: GridProto ()
helloWorld = defaultGridProto
  { title = "Hello World"
  , viewFn = \_ ->
      drawString wh1 (0,0) "hello" <> drawString wh1 (0,1) "world"
  }
````

## API

```haskell
-- The main type with config and callbacks
data GridProto state = GridProto
  -- Window name
  { title :: String
  -- Grid size
  , rows :: Int -- Default: 18
  , cols :: Int -- Default: 32
  -- Scaling and clear color
  , tilePixelSize :: Int -- Default: 24 minimum for readable text
  , backgroundColor :: Color -- Default: Black2
  -- Callbacks which are called each frame
  , updateFn :: Input -> state -> IO (state, [String]) -- [String] is user logging
  , viewFn :: state -> View
  , sfxFn :: state -> [Sfx]
  , quitFn :: state -> Bool
  }

defaultGridProto :: GridProto s -- Easy to update fields

runGridProto -- Loops until exited
  :: GridProto s -- Main type
  -> s -- Initial state
  -> IO ()

--
-- View
--

type View = Map (Int, Int) Tile -- Map (X,Y) Tile

emptyView :: View

data Tile = Tile
  { symbol :: Maybe (Char, Color) -- At front
  , shape :: Maybe (Shape, Color) -- In middle
  , fill :: Maybe Color -- At back
  }

data Color -- 15 base colors with 3 variations from tint, regular, and shaded. 45 Total.
-- Shorten constructors
-- Tint Reg. Shade
   rd0, rd1, rd2 -- Red
   or0, or1, or2 -- Orange
   yw0, yw1, yw2 -- Yellow
   ch0, ch1, ch2 -- Chartreuse
   gn0, gn1, gn2 -- Green
   sp0, sp1, sp2 -- Spring
   cn0, cn1, cn2 -- Cyan
   az0, az1, az2 -- Azure
   bu0, bu1, bu2 -- Blue
   vt0, vt1, vt2 -- Violet
   mg0, mg1, mg2 -- Magenta
   rs0, rs1, rs2 -- Rose
   br0, br1, br2 -- Brown
   gy0, gy1, gy2 -- Gray
   wh0, wh1, wh2 -- White
   bk0, bk1, bk2 -- Black

data Shape
  = Circle | FillCircle
  | Triangle | FillTriangle
  | Square | FillSquare
  | Plus | Dash
  | Bar | Cross

drawString :: Color -> (Int, Int) -> String -> View
drawTile :: View -> (Int, Int) -> Tile -> View

drawView :: View -> (Int, Int) -> View -> View
mergeView :: View -> View -> View
mergeViews :: [View] -> View

rainbow, warms, cools, colorWheel0, colorWheel1, colorWheel2 :: [Color]
shade, tint :: Color -> Color

data Viewport = Viewport
  { vpView :: View
  , vpXY :: (Int, Int)
  , vpDim :: (Int, Int)
  }

mergeViewport :: View -> Viewport -> View
mergeViewports :: View -> [Viewport] -> View

-- instances of MapTile on Tile, View, Viewport
mapTile :: MapTile a
  => ((Char, Color) -> (Char, Color)) -> ((Shape, Color) -> (Shape, Color))
  -> (Color -> Color) -> a -> a
mapSymbol :: MapTile a => ((Char, Color) -> (Char, Color)) -> a -> a
mapShape :: MapTile a => ((Shape, Color) -> (Shape, Color)) -> a -> a
mapFill :: MapTile a => (Color -> Color) -> a -> a

--
-- Input
--

data Input = Input
  { mouse :: Mouse, keys :: Keys
  , controller1, controller2, controller3, controller4 :: Controller
  }

data Key
  = Char Char
  | UpArrow | DownArrow | LeftArrow | RightArrow
  | Enter | Escape
  | LeftShift | RightShift | LeftControl | RightControl
  | LeftAlt | RightAlt
  | Tab | Backspace | Meta

data KeyState = Pressed | Held | Released | Untouched

newtype Keys

lookupKey :: Keys -> Key -> KeyState

data Mouse = Mouse { mousePosition :: (Int, Int), mouseButton :: KeyState }

data Controller = Controller
  { isConnected :: Bool
  , startButton, backButton :: KeyState
  , dpadUp, dpadDown, dpadLeft, dpadRight :: KeyState
  , aButton, bButton, xButton, yButton :: KeyState
  , leftStick , rightStick :: KeyState
  , leftShoulder, rightShoulder :: KeyState
  , leftAxis, rightAxis :: Axis
  }

data Axis = Axis { xAxis :: Float, yAxis :: Float }

--
-- Sfx
--

data Sfx
  = SfxSuccess | SfxBell | SfxSelect
  | SfxNoSelect | SfxScroll | SfxChimes
  | SfxLaser | SfxPowerUp | SfxJump
  | SfxDamage | SfxExplosion | SfxNoise

```

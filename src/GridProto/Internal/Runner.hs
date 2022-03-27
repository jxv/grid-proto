{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module GridProto.Internal.Runner
  ( GridProto(..)
  , runGridProto
  , defaultGridProto
  ) where

import qualified Data.Map as Map
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import qualified SDL
import qualified SDL.Raw.Event as Event
import qualified SDL.Font as Font
import qualified SDL.Mixer as Mixer
import qualified SDL.Primitive as Gfx

import Prelude hiding (lookup)
import GHC.Generics (Generic)
import Control.Applicative ((<|>))
import Data.IORef
import Data.Map (Map, fromList, (!), delete, alter, insert, filterWithKey, member, notMember, toList)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Function (fix)
import Data.Foldable (forM_)
import Data.Traversable (forM)
import Data.Semigroup (Semigroup(..))
import Data.Text (pack)
import Data.Word (Word8)
import Data.StateVar (($=))
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import SDL.Input.Keyboard.Codes
import System.Mem (performGC)

import GridProto.Internal.Core
import GridProto.Internal.Font
import GridProto.Internal.Sfx

data GridProto s = GridProto
  { title :: String
  , rows :: Int
  , cols :: Int
  , tilePixelSize :: Int
  , backgroundColor :: Color
  , updateFn :: Input -> s -> IO (s, [String])
  , viewFn :: s -> View
  , sfxFn :: s -> [Sfx]
  , quitFn :: s -> Bool
  }

defaultGridProto :: GridProto s
defaultGridProto = GridProto
  { title = "Grid Proto"
  , rows = 18
  , cols = 32
  , tilePixelSize = 16
  , backgroundColor = Black2
  , updateFn = \_ s -> return (s, [])
  , viewFn = \_ -> emptyView
  , sfxFn = \_ -> []
  , quitFn = \_ -> False
  }

runGridProto :: GridProto s -> s -> IO ()
runGridProto GridProto
  { title
  , rows
  , cols
  , tilePixelSize = tps
  , backgroundColor
  , updateFn
  , sfxFn
  , viewFn
  , quitFn
  }
  initialState
  = do
  SDL.initialize [SDL.InitVideo, SDL.InitAudio, SDL.InitGameController]
  Font.initialize
  Mixer.openAudio Mixer.defaultAudio 256
  let windowW = num (cols * tps)
  let windowH = num (rows * tps)
  window <- SDL.createWindow (pack title) SDL.defaultWindow
    { SDL.windowInitialSize = V2 windowW windowH
    , SDL.windowHighDPI = True
    }
  wsize@(V2 ww wh) <- SDL.glGetDrawableSize window
  let tilePixelSize = num $ ww * num tps `div` windowW
  let winDensity = fromIntegral tilePixelSize / fromIntegral tps :: Float
  --
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  numJoysticks <- SDL.numJoysticks
  joystickDevices <- SDL.availableJoysticks
  gameControllerIds <- fmap (fmap snd . V.filter fst) $ forM joystickDevices $ \jd -> do
    let jdId = SDL.joystickDeviceId jd
    (,) <$> Event.isGameController jdId <*> pure jdId
  gameControllers <- mapM Event.gameControllerOpen gameControllerIds
  sfxData <- loadSfxData
  (font, fontSize) <- loadFont renderer tilePixelSize
  fontColorMapRef <- newFontColorMap
  let findSymbols' = findSymbols renderer font fontSize fontColorMapRef
  let initInput = Input
        (Mouse (0,0) Untouched)
        (Keys Map.empty)
        initController { isConnected = elem 0 gameControllerIds }
        initController { isConnected = elem 1 gameControllerIds }
        initController { isConnected = elem 2 gameControllerIds }
        initController { isConnected = elem 3 gameControllerIds }
  sfxChannels <- newIORef $ cycle [0..7]
  ($ (initialState, initInput)) $ fix $ \loop (state, input) -> do
    ticks <- startFrame
    let quit = quitFn state
    events <- SDL.pollEvents
    (SDL.P mousePos) <- SDL.getAbsoluteMouseLocation
    let (V2 mouseX mouseY) = fmap floor $ (num <$> mousePos) * pure winDensity
    let mouseTilePos = tileByMousePosition tilePixelSize (mouseX, mouseY) (rows, cols)
    mouseClick <- ($ SDL.ButtonLeft) <$> SDL.getMouseButtons
    let eventPayloads = map SDL.eventPayload events
    let input' = makeInput input mouseTilePos mouseClick eventPayloads
    if quit || elem SDL.QuitEvent eventPayloads
      then return ()
      else do
        (state', log) <- updateFn input' state
        let sfxs = sfxFn state'
        let view = viewFn state'
        SDL.rendererDrawColor renderer $= sdlColor backgroundColor
        SDL.clear renderer
        drawTileMap backgroundColor renderer tilePixelSize findSymbols' view
        playSfxs sfxData sfxChannels sfxs
        SDL.present renderer
        mapM_ putStrLn log
        performGC
        endFrame 60 ticks
        loop (state', input')
  mapM_ Event.gameControllerClose gameControllers
  Font.free font
  SDL.destroyWindow window
  freeSfxData sfxData
  Mixer.quit
  Font.quit
  SDL.quit

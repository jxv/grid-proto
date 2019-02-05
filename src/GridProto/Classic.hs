{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module GridProto.Classic
  ( Classic(..)
  , runClassic
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
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types (FromJSONKey, ToJSONKey)
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
import SDL.FPS
import SDL.Input.Keyboard.Codes
import System.Mem (performGC)

import GridProto.Internal.Core
import GridProto.Internal.Font
import GridProto.Internal.SfxAttention

data Classic s = Classic
  { title :: String
  , rows :: Int
  , cols :: Int
  , tilePixelSize :: Int
  , backgroundColor :: Color
  , setupFn :: IO s
  , updateFn :: Input -> s -> IO s
  , cleanupFn :: s -> IO ()
  , tileMapFn :: s -> Map (Int, Int) Tile
  , sfxFn :: s -> [Sfx]
  , quitFn :: s -> Bool
  }

runClassic :: Classic s -> IO ()
runClassic Classic
  { title
  , rows
  , cols
  , tilePixelSize
  , backgroundColor
  , setupFn
  , updateFn
  , sfxFn
  , tileMapFn
  , quitFn
  }
  = do
  SDL.initialize [SDL.InitVideo, SDL.InitAudio, SDL.InitGameController]
  Font.initialize
  Mixer.openAudio Mixer.defaultAudio 256
  window <- SDL.createWindow (pack title) SDL.defaultWindow { SDL.windowInitialSize = V2 (num $ rows * tilePixelSize) (num $ cols * tilePixelSize) }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  numJoysticks <- SDL.numJoysticks
  joystickDevices <- SDL.availableJoysticks
  gameControllerIds <- fmap (fmap snd . V.filter fst) $ forM joystickDevices $ \jd -> do
    let jdId = SDL.joystickDeviceId jd
    (,) <$> Event.isGameController jdId <*> pure jdId
  gameControllers <- mapM Event.gameControllerOpen gameControllerIds
  font <- loadFont renderer tilePixelSize
  fontMapRef <- newFontMap
  attention <- Mixer.decode sfxAttentionData
  let findSymbol' = findSymbol renderer font fontMapRef
  initialState <- setupFn
  let initInput = Input (Mouse (0,0) Untouched) (Keys Map.empty) initController
  ($ (initialState, initInput)) $ fix $ \loop (state, input) -> do
    ticks <- startFrame
    let quit = quitFn state
    events <- SDL.pollEvents
    (SDL.P mousePos) <- SDL.getAbsoluteMouseLocation
    let (V2 mouseX mouseY) = num <$> mousePos
    let mouseTilePos = tileByMousePosition tilePixelSize (mouseX, mouseY) (rows, cols)
    mouseClick <- ($ SDL.ButtonLeft) <$> SDL.getMouseButtons
    let eventPayloads = map SDL.eventPayload events
    let input' = makeInput input mouseTilePos mouseClick eventPayloads
    if quit || elem SDL.QuitEvent eventPayloads
      then return ()
      else do
        state' <- updateFn input state
        let sfxs = sfxFn state'
        let tileMap = tileMapFn state'
        SDL.rendererDrawColor renderer $= sdlColor backgroundColor
        SDL.clear renderer
        drawTileMap backgroundColor renderer tilePixelSize findSymbol' tileMap
        playSfxs attention sfxs
        SDL.present renderer
        performGC
        endFrame 60 ticks
        loop (state', input')
  mapM_ Event.gameControllerClose gameControllers
  Font.free font
  SDL.destroyWindow window
  Mixer.quit
  Font.quit
  SDL.quit


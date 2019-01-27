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
import qualified SDL
import qualified SDL.Font as Font
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
import Data.Semigroup (Semigroup(..))
import Data.Text (pack)
import Data.Word (Word8)
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import SDL.Input.Keyboard.Codes

import GridProto.Internal.Core
import GridProto.Internal.Font

data Classic s = Classic
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

runClassic :: Classic s -> IO ()
runClassic Classic
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
  Font.initialize
  window <- SDL.createWindow (pack title) SDL.defaultWindow { SDL.windowInitialSize = V2 (num $ rows * cellPixelSize) (num $ cols * cellPixelSize) }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  font <- Font.decode fontData ((cellPixelSize * 2) `div` 3)
  initialState <- setupFn
  let initInput = Input Idle Map.empty
  ($ (initialState, initInput)) $ fix $ \loop (state, input) -> do
    let quit = quitFn state
    events <- SDL.pollEvents
    (SDL.P mousePos) <- SDL.getAbsoluteMouseLocation
    let (V2 mouseX mouseY) = num <$> mousePos
    let mouseCellPos = cellByMousePosition cellPixelSize (mouseX, mouseY) (rows, cols)
    mouseClick <- ($ SDL.ButtonLeft) <$> SDL.getMouseButtons
    let eventPayloads = map SDL.eventPayload events
    let input' = makeInput (keys input) mouseCellPos mouseClick eventPayloads
    if quit || elem SDL.QuitEvent eventPayloads
      then return ()
      else do
        state' <- updateFn input state
        let cellMap = cellsFn state'
        SDL.clear renderer
        drawCellMap backgroundColor renderer cellPixelSize cellMap
        SDL.present renderer
        loop (state', input')
  SDL.destroyWindow window
  Font.free font
  Font.quit
  SDL.quit


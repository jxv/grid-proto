module Main where

import GridProto.Classic
import GridProto.Core
import Data.Maybe (catMaybes)

main :: IO ()
main = runClassic classic

classic :: Classic [Sfx]
classic = Classic
  { title = "Sound Example"
  , rows = 2
  , cols = 10
  , tilePixelSize = 24
  , backgroundColor = Black2
  , setupFn = return []
  , updateFn = update
  , cleanupFn = const (return ())
  , tileMapFn = const mempty
  , sfxFn = id
  , quitFn = const False
  }

update :: Input -> [Sfx] -> IO [Sfx]
update Input{keys=keys} _ = return $ catMaybes [achievement, gong, door]
  where
    isPressed c = lookupKey keys (Char c) == Pressed
    chooseSound c sfx = if isPressed c then Just sfx else Nothing
    achievement = chooseSound '1' SfxAchievement
    gong = chooseSound '2' SfxGong
    door = chooseSound '3' SfxDoor

tileMap :: [Sfx] -> Map (Int, Int) Tile
tileMap _ = fromList []


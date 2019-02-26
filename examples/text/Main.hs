module Main where

import GridProto.Classic
import GridProto.Core

main :: IO ()
main = runClassic classic

classic :: Classic ()
classic = Classic
  { title = "Text Example"
  , rows = sides
  , cols = sides
  , tilePixelSize = 64
  , backgroundColor = Black2
  , setupFn = return ()
  , updateFn = \_ _ -> return ()
  , cleanupFn = const (return ())
  , tileMapFn = const tileMap
  , sfxFn = const []
  , quitFn = const False
  }
  where
    sides = 8


tileMap :: Map (Int, Int) Tile
tileMap = fromList $ line 0 "hello" ++ line 1 "world"
  where
    line y str = zipWith (tile y) [0..] str
    tile y x sym = ((x, y), Tile (Just (sym, wh1)) Nothing Nothing)


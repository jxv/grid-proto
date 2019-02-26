module Main where

import GridProto.Classic
import GridProto.Core

main :: IO ()
main = runClassic classic

classic :: Classic (Int, Int, Color)
classic = Classic
  { title = "Controller Example"
  , rows = sides
  , cols = sides
  , tilePixelSize = 64
  , backgroundColor = Black2
  , setupFn = return (halfSide, halfSide, Red0)
  , updateFn = update
  , cleanupFn = const (return ())
  , tileMapFn = tileMap
  , sfxFn = const []
  , quitFn = const False
  }
  where

sides, halfSide :: Int
sides = 9
halfSide = sides `div` 2

update :: Input -> (Int, Int, Color) -> IO (Int, Int, Color)
update input (_,_,color) = return $ case lookupMap 0 (controllers input) of
  Nothing -> (0,0,color)
  Just c -> let
    color' = case (aButton c, bButton c, xButton c, yButton c) of
      (Pressed,_,_,_) -> Red0
      (_,Pressed,_,_) -> Blue0
      (_,_,Pressed,_) -> Green0
      (_,_,_,Pressed) -> Yellow0
      _ -> color
    axis = leftAxis c
    adjust v = halfSide + round (fromIntegral halfSide * v)
    in (adjust (xAxis axis), adjust (yAxis axis), color')

tileMap :: (Int, Int, Color) -> Map (Int, Int) Tile
tileMap (x,y,color) = fromList [((x,y), Tile Nothing shape Nothing)]
  where
    shape = Just $ (FillCircle, color)

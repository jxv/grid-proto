module Main where

import GridProto.Classic
import GridProto.Core

main :: IO ()
main = runClassic classic

classic :: Classic (Int, Int, Bool)
classic = Classic
  { title = "Hello World"
  , rows = sides
  , cols = sides
  , tilePixelSize = 128
  , backgroundColor = Black2
  , setupFn = return (0, 0, False)
  , updateFn = update
  , cleanupFn = const (return ())
  , tileMapFn = tileMap sides
  , quitFn = quit
  }
  where
    sides = 8

update :: Input -> (Int, Int, Bool) -> IO (Int, Int, Bool)
update input state = case mouse input of
  Hover (mx,my) -> return (mx,my, lookupMap Escape (keys input) == Just Released)
  Click (mx,my) -> return (mx,my, mx == 0 && my == 0)
  _ -> return state

tileMap :: Int -> (Int, Int, Bool) -> Map (Int, Int) Tile
tileMap sides (mx,my,click) = fromList $ do
  y <- [0..(sides - 1)]
  x <- [0..(sides - 1)]
  let color = Just $
        if (mx,my) == (x,y)
          -- Mouse color
          then if click then Green1 else Red1
          -- Alternate background colors
          else if (x + y) `mod` 2 == 0 then Brown1 else Brown2
  let shape =
        if (x + y) `mod` 2 == 1
          then Nothing
          else if y >= 0 && y <= 2
            then Just (Circle,  White1)
            else if y >= 5 && y <= 7
              then Just (Circle, Black1)
              else Nothing
  return ((x,y), Tile (Just ('`', Green1)) shape color)


quit :: (Int, Int, Bool) -> Bool
quit (_,_,True) = True
quit _ = False

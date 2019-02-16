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
  , tilePixelSize = 64
  , backgroundColor = Black2
  , setupFn = return (0, 0, False)
  , updateFn = update
  , cleanupFn = const (return ())
  , tileMapFn = tileMap sides
  , sfxFn = \(x,y,_) -> if x == 7 && y == 7 then [Attention] else []
  , quitFn = quit
  }
  where
    sides = 8

update :: Input -> (Int, Int, Bool) -> IO (Int, Int, Bool)
update Input{mouse=Mouse{mousePosition=(mx,my),mouseButton=mouseButton},keys=keys} _
  | mouseButton == Pressed = return (mx,my, mx == 0 && my == 0)
  | otherwise = return (mx, my, lookupKey keys Escape == Released)

tileMap :: Int -> (Int, Int, Bool) -> Map (Int, Int) Tile
tileMap sides (mx,my,click) = fromList $ do
  y <- [0..(sides - 1)]
  x <- [0..(sides - 1)]
  let color = Just $
        if (mx,my) == (x,y)
          -- Mouse color
          then if click then Green1 else Green2
          -- Alternate background colors
          else if (x + y) `mod` 2 == 0 then Chartreuse1 else Chartreuse2
  let (symbol, shape) =
        if (x + y) `mod` 2 == 1
          then (Nothing, Nothing)
          else if y >= 0 && y <= 2
            then (Just ('W', White1), Just (Circle,  White1))
            else if y >= 5 && y <= 7
              then (Just ('B', Black1), Just (Circle, Black1))
              else (Nothing, Nothing)
  return ((x,y), Tile symbol shape color)


quit :: (Int, Int, Bool) -> Bool
quit (_,_,True) = True
quit _ = False

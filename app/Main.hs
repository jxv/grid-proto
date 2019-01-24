module Main where

import GridProto

main :: IO ()
main = runGridProto gp

gp :: GridProto (Int, Int, Bool)
gp = GridProto
  { title = "Hello World"
  , rows = sides
  , cols = sides
  , cellPixelSize = 64
  , backgroundColor = Black
  , setupFn = return (0, 0, False)
  , updateFn = update
  , cleanupFn = const (return ())
  , cellsFn = cells sides
  , quitFn = quit
  }
  where
    sides = 8

update :: Input -> (Int, Int, Bool) -> IO (Int, Int, Bool)
update input state = case mouse input of
  Hover (mx,my) -> return (mx,my, lookupMap DownArrow (keys input) == Just Released)
  Click (mx,my) -> return (mx,my, True)
  _ -> return state

cells :: Int -> (Int, Int, Bool) -> Map (Int, Int) Cell
cells sides (mx,my,click) = fromList $ do
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
            then Just (FillCircle,  White)
            else if y >= 5 && y <= 7
              then Just (FillCircle, Black)
              else Nothing
  return ((x,y), Cell shape color)


quit :: (Int, Int, Bool) -> Bool
quit (0,0,True) = True
quit _ = False

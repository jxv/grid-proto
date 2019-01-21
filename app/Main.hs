module Main where

import GridProto

main :: IO ()
main = runGridProto gp

gp :: GridProto (Int, Int, Bool)
gp = GridProto
  { title = "Hello World"
  , rows = sides
  , cols = sides
  , cellPixelSize = 32
  , setupFn = return (0, 0, False)
  , updateFn = update
  , cleanupFn = const (return ())
  , cellsFn = cells sides
  , quitFn = quit
  }
  where
    sides = 32

update :: Input -> (Int, Int, Bool) -> IO (Int, Int, Bool)
update input state = case mouse input of
  Hover (mx,my) -> return (mx,my,False)
  Click (mx,my) -> return (mx,my,True)
  _ -> return state

cells :: Int -> (Int, Int, Bool) -> Map (Int, Int) Cell
cells sides (mx,my,click) = fromList $ do
  y <- [0..(sides - 1)]
  x <- [0..(sides - 1)]
  let color =
        if (mx,my) == (x,y)
          -- Mouse color
          then if click then Green else Red
          -- Alternate background colors
          else if (x + y) `mod` 2 == 0 then DarkGray else Gray
  return ((x,y), Cell (Just (Square, DarkBlue)) color)

quit :: (Int, Int, Bool) -> Bool
quit (0,0,True) = True
quit _ = False

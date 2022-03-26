module Arrows where

import GridProto

main :: IO ()
main = runGridProto example (0, 0)

example :: GridProto (Int, Int)
example = defaultGridProto
  { title = "Arrows Example"
  , updateFn = update
  , viewFn = view
  }

update :: Input -> (Int, Int) -> IO ((Int, Int), [String])
update Input{keys=keys} (x,y) = return ((x',y'), [])
  where
    x'
      | lookupKey keys LeftArrow == Pressed = x - 1
      | lookupKey keys RightArrow == Pressed = x + 1
      | otherwise = x
    y'
      | lookupKey keys UpArrow == Pressed = y - 1
      | lookupKey keys DownArrow == Pressed = y + 1
      | otherwise = y

view :: (Int, Int) -> View
view xy = fromList [(xy, Tile Nothing Nothing (Just rd0))]


module Text where

import GridProto

main :: IO ()
main = runGridProto example ()

example :: GridProto ()
example = defaultGridProto
  { title = "Text Example"
  , viewFn = const view
  }

view :: View
view = fromList $ line 0 "hello" ++ line 1 "world"
  where
    line y str = zipWith (tile y) [0..] str
    tile y x sym = ((x, y), Tile (Just (sym, wh1)) Nothing Nothing)


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
view = drawString wh1 (0,0) "hello" <> drawString wh1 (0,1) "world"

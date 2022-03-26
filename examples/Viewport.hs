module Viewport where

import GridProto

type State = (Bool, (Int, Int), (Int,Int))

main :: IO ()
main = runGridProto example (False, (0,0), (10,10))

example :: GridProto State
example = defaultGridProto
  { title = "Viewport Example"
  , updateFn = update
  , viewFn = view
  }

update :: Input -> State -> IO (State, [String])
update Input{mouse=Mouse{mousePosition=mxy,mouseButton=mouseButton}} st@(color,xy0,xy1)
  | mouseButton == Pressed = return (if color then (not color, mxy, xy1) else (not color, xy0, mxy), [])
  | otherwise = return (st, [])

makeViewport :: (Int, Int) -> (Int, Int) -> (Color, Color) -> Viewport
makeViewport xy (w,h) (a,b) = Viewport tiles xy (w,h)
  where
    tiles = fromList $ do
      y <- [0..(h - 1)]
      x <- [0..(w - 1)]
      let color = Just $ if (x + y) `mod` 2 == 0 then a else b
      let (symbol, shape) = (Nothing, Nothing)
      return ((x,y), Tile symbol shape color)

view :: State -> View
view (color,xy0,xy1) = mergeViewports emptyView
  [ mapFill (if color then shade else id)     $ makeViewport xy0 (10,15) (Red0, Red1)
  , mapFill (if not color then shade else id) $ makeViewport xy1 (15,7)  (Blue0, Blue1)
  ]

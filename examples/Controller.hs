module Controller where

import GridProto

main :: IO ()
main = runGridProto example (halfSide, halfSide, Red0)

sides, halfSide :: Int
sides = 9
halfSide = sides `div` 2

example :: GridProto (Int, Int, Color)
example = defaultGridProto
  { title = "Controller Example"
  , rows = sides
  , cols = sides
  , backgroundColor = Black2
  , updateFn = update
  , viewFn = view
  }

update :: Input -> (Int, Int, Color) -> IO ((Int, Int, Color), [String])
update input (_,_,color) = return $ let
  color' = case (aButton c, bButton c, xButton c, yButton c) of
    (Pressed,_,_,_) -> Red0
    (_,Pressed,_,_) -> Blue0
    (_,_,Pressed,_) -> Green0
    (_,_,_,Pressed) -> Yellow0
    _ -> color
  axis = leftAxis c
  adjust v = halfSide + round (fromIntegral halfSide * v)
  in ((adjust (xAxis axis), adjust (yAxis axis), color'), [])
  where
    c = controller1 input

view :: (Int, Int, Color) -> View
view (x,y,color) = fromList [((x,y), Tile Nothing shape Nothing)]
  where
    shape = Just $ (FillCircle, color)

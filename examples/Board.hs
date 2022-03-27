module Board where

import GridProto

type State = (Int, Int, Bool) -- (x, y, quit?)

main :: IO ()
main = runGridProto example (0, 0, False)

example :: GridProto State
example = defaultGridProto
  { title = "Board Example"
  , rows = 8
  , cols = 8
  , tilePixelSize = 32
  , updateFn = update
  , viewFn = view 8
  , quitFn = quit
  }

update :: Input -> State -> IO (State, [String])
update Input{mouse=Mouse{mousePosition=(mx,my),mouseButton=mouseButton},keys=keys} _
  | mouseButton == Pressed = return ((mx,my, mx == 0 && my == 0), [])
  | otherwise = return ((mx, my, lookupKey keys Escape == Released), [])

view :: Int -> State -> View
view sides (mx,my,click) = fromList $ do
  y <- [0..(sides - 1)]
  x <- [0..(sides - 1)]
  let color = Just $
        if (mx,my) == (x,y)
          -- Mouse color
          then if click then Green1 else Red1
          -- Alternate background colors
          else if (x + y) `mod` 2 == 0 then Brown1 else Brown2
  let (symbol, shape) =
        if (x + y) `mod` 2 == 1
          then (Nothing, Nothing)
          else if y >= 0 && y <= 2
            then (Just ('W', Black1), Just (FillCircle, White1))
            else if y >= 5 && y <= 7
              then (Just ('B', White1), Just (FillCircle, Black1))
              else (Nothing, Nothing)
  return ((x,y), Tile symbol shape color)


quit :: State -> Bool
quit (_,_,q) = q

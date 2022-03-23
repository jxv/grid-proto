module Sound where

import GridProto
import Data.Maybe (catMaybes)

main :: IO ()
main = runGridProto example []

example :: GridProto [Sfx]
example = defaultGridProto
  { title = "Sound Example"
  , updateFn = update
  , sfxFn = id
  }

update :: Input -> [Sfx] -> IO ([Sfx], [String])
update Input{keys=keys} _ = return (sfxs, [])
  where
    isPressed c = lookupKey keys (Char c) == Pressed
    chooseSound c sfx = if isPressed c then Just sfx else Nothing
    inputs = ['1'..'9'] ++ ['0'] ++ ['q','w','r','t','y','u','i','o','p']
    sfxs = catMaybes $ zipWith chooseSound inputs [minBound..maxBound]

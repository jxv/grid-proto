{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.List (foldl')
import GridProto.Classic
import GridProto.Core

main :: IO ()
main = runClassic classic

classic :: Classic St
classic = Classic
  { title = "Tactics"
  , rows = 15
  , cols = 15
  , tilePixelSize = 24
  , backgroundColor = Black2
  , setupFn = return initState
  , updateFn = update
  , cleanupFn = const (return ())
  , tileMapFn = tileMap
  , sfxFn = const []
  , quitFn = const False
  }

update :: Input -> St -> IO St
update Input{mouse=Mouse{mousePosition},ticks} st = do
  return st
    { stGame = changeTurn P'Red $ tick (stGame st)
    , stMouse = mousePosition
    }
  where
    tick g = g { gTick = ticks }

changeTurn :: P -> G -> G
changeTurn p g = g { gUnit = activatePlayer p (gUnit g) }

-- Player
data P
  = P'Red
  | P'Blue
  deriving (Show, Eq)

playerUnitColor :: Bool -> P -> Color
playerUnitColor True = \case
  P'Red -> rd2
  P'Blue -> bu2
playerUnitColor False = \case
  P'Red -> rd0
  P'Blue -> bu0

playerSymbolColor :: P -> Color
playerSymbolColor = \case
  P'Red -> rd0
  P'Blue -> bu0

-- Unit Class
data C
  = C'Infantry
  | C'Mech
  | C'Recon
  | C'Tank
  deriving (Show, Eq)

unitClassShape :: C -> Shape
unitClassShape = \case
  C'Infantry -> Circle
  C'Mech -> FillCircle
  C'Recon -> Square
  C'Tank -> FillSquare

-- Unit
data U = U 
  { uClass :: C
  , uHealth :: Int
  , uActive :: Bool
  , uOwner :: P
  } deriving (Show, Eq)

-- Square
data S = S
  { sUnit :: Maybe U
  , sTerrain :: T
  , sBuilding :: Maybe B
  } deriving (Show, Eq)

squareTile :: Bool -> S -> Tile
squareTile flash S{sTerrain,sUnit,sBuilding} = Tile
  (symbolSquare flash sUnit sBuilding)
  (unitShape flash <$> sUnit)
  (Just (terrainColor sTerrain))

symbolSquare :: Bool -> Maybe U -> Maybe B -> Maybe (Char, Color)
symbolSquare flash u b = if flash then (u' <|> b') else (b' <|> u')
  where
    u' = symbolUnit =<< u
    b' = symbolBuilding <$> b

symbolUnit :: U -> Maybe (Char, Color)
symbolUnit U{uHealth,uOwner}
  | uHealth < 10 && uHealth >= 0 = Just (head $ show uHealth, wh1) -- playerSymbolColor uOwner)
  | otherwise = Nothing

symbolBuilding :: B -> (Char, Color)
symbolBuilding b =
  (case bClass b of
    K'HQ -> 'H'
    K'City -> 'C'  
  , fromMaybe gy0 $ playerSymbolColor <$> (bOwner b) 
  )

unitShape :: Bool -> U -> (Shape, Color)
unitShape flash U{uActive,uClass,uOwner} = (unitClassShape uClass, playerUnitColor (flash && uActive) uOwner)

-- Terrain
data T
  = T'Grass
  | T'Water
  | T'Mountain
  | T'Sand
  | T'Forest
  deriving (Show, Eq)

gr, wa, mt, sd, fr :: T
gr = T'Grass
wa = T'Water
mt = T'Mountain
sd = T'Sand
fr = T'Forest

terrainColor :: T -> Color
terrainColor T'Grass = ch2
terrainColor T'Water = cn0
terrainColor T'Mountain = vt1
terrainColor T'Sand = br0 
terrainColor T'Forest = gn2

-- Building Class
data K
  = K'HQ
  | K'City
  deriving (Show, Eq)

-- Building
data B = B
  { bClass :: K
  , bOwner :: Maybe P
  , bCapture :: Maybe (P, Int)
  } deriving (Show, Eq)

-- Game
data G = G
  { gUnit :: Map (Int, Int) U
  , gTerrain :: Map (Int, Int) T
  , gBuilding :: Map (Int, Int) B
  , gTick :: Int
  , gTurn :: P
  } deriving (Show, Eq)

--
squares :: G -> [((Int, Int), S)] 
squares G{gTerrain,gUnit,gBuilding} = map (\(idx, t) -> (idx, S (lookupMap idx gUnit) t (lookupMap idx gBuilding))) $ toList gTerrain

genIndices :: (Int,Int) -> [(Int,Int)]
genIndices (w,h) = do
  y <- [0..(pred h)]
  x <- [0..(pred w)]
  pure (x,y)

terrain1 :: Map (Int, Int) T
terrain1 = fromList $ zip (genIndices (10,10))
  [ mt, mt, mt, mt, mt, mt, mt, mt, mt, mt -- 0
  , mt, gr, gr, gr, gr, gr, gr, gr, gr, mt -- 1
  , mt, gr, gr, gr, gr, gr, gr, gr, gr, mt -- 2
  , mt, gr, gr, gr, gr, gr, gr, gr, gr, mt -- 3
  , mt, gr, gr, gr, gr, gr, gr, gr, gr, mt -- 4
  , mt, gr, gr, gr, gr, gr, gr, gr, gr, mt -- 5
  , mt, gr, gr, gr, gr, gr, gr, gr, gr, mt -- 6
  , mt, gr, gr, gr, gr, gr, gr, gr, gr, mt -- 7
  , mt, sd, wa, fr, gr, gr, gr, gr, gr, mt -- 8
  , mt, mt, mt, mt, mt, mt, mt, mt, mt, mt -- 9
  ]

game1 :: G
game1 = G
  (fromList
    [ ((0,0), U C'Infantry 9 True P'Red)
    , ((1,1), U C'Mech 9 True P'Red)
    , ((2,2), U C'Recon 9 True P'Red)
    , ((3,3), U C'Tank 9 True P'Red)
    , ((0,1), U C'Infantry 9 True P'Blue)
    , ((1,2), U C'Mech 9 True P'Blue)
    , ((2,3), U C'Recon 9 True P'Blue)
    , ((3,4), U C'Tank 9 True P'Blue)
    ])
  terrain1
  (fromList [((1,1), B K'HQ Nothing Nothing)])
  0
  P'Red

gameTiles :: G -> Map (Int, Int) Tile
gameTiles g = fromList . map (\(idx,s) -> (idx, squareTile (tickFlash g) s)) $ squares g

tileMap :: St -> Map (Int, Int) Tile
tileMap st = flashMovement $ flashMouse $ gameTiles (stGame st)
  where
    flashMouse = if flash then insert (stMouse st) whiteTile else id
    flashMovement = if  tickFlashSlow  (stGame st)then flip mergeTiles (movementTiles $ stMouse st) else id
    flash = tickFlashFast (stGame st)
    whiteTile = Tile Nothing Nothing (Just wh1)

-- hacks

manhattanDirs :: (Int, Int) -> (Int, Int) -> [D]
manhattanDirs a b = go a b Nothing
  where
    go xy@(x,y) xy'@(x',y') last
      | xy == xy' = fromMaybe [] (fmap return last)
      | abs (x - x') >= abs (y - y') = let
          d = if x' > x then rt else lf
          in  d : go (dirPos d xy) xy' (Just d)
      | abs (x - x') <  abs (y - y') = let
          d = if y' > y then dn else up
          in  d : go (dirPos d xy) xy' (Just d)
      | otherwise = []

--

movementTiles :: (Int, Int) -> Map (Int, Int) Tile
movementTiles xy = snakeTiles $ snake (7,7) (manhattanDirs (7,7) xy)

initSafe :: [a] -> [a]
initSafe [] = []
initSafe xs = init xs

snake :: (Int,Int) -> [D] -> [((Int,Int), A)]
snake xy dirs = initSafe . (\(_,_,ls) -> ls) $ foldl' f (xy,Nothing,[]) dirs
  where
    f
      :: ((Int,Int), Maybe D, [((Int, Int), A)])
      -> D
      ->  ((Int,Int), Maybe D, [((Int, Int), A)]) 
    f (xy, d', ls) d = (xy', Just d, (xy,a):ls)
      where
        xy' = dirPos d xy
        a = dirsAngle (fromMaybe d d') d

dirPos :: D -> (Int, Int) -> (Int, Int)
dirPos d (x,y) = case d of
  D'U -> (x,y-1)
  D'D -> (x,y+1)
  D'L -> (x-1,y)
  D'R -> (x+1,y)

snakeTiles :: [((Int, Int), A)] -> Map (Int, Int) Tile
snakeTiles = fmap tileFromAngle . fromList

tileFromAngle :: A -> Tile
tileFromAngle a = Tile (Just (charFromAngle a, yw0)) Nothing (Just cn2)

charFromAngle :: A -> Char
charFromAngle = \case
  VT -> '|'
  HZ -> '-'
  Q1 -> '┏'
  Q2 -> '┓'
  Q3 -> '┗'
  Q4 -> '┛'

data A
  = VT
  | HZ
  | Q1
  | Q2
  | Q3
  | Q4
  deriving (Show, Eq)

data D
  = D'U
  | D'D
  | D'L
  | D'R
  deriving (Show, Eq)

up, dn, lf, rt :: D
up = D'U
dn = D'D
lf = D'L
rt = D'R

dirsAngle :: D -> D -> A
dirsAngle from to = case (from,to) of
  (D'U, D'U) -> VT
  (D'U, D'D) -> VT
  (D'U, D'L) -> Q2
  (D'U, D'R) -> Q1
  --  
  (D'D, D'D) -> VT
  (D'D, D'U) -> VT
  (D'D, D'L) -> Q4
  (D'D, D'R) -> Q3
  --
  (D'R, D'R) -> HZ
  (D'R, D'L) -> HZ
  (D'R, D'U) -> Q4
  (D'R, D'D) -> Q2
  --
  (D'L, D'L) -> HZ
  (D'L, D'R) -> HZ
  (D'L, D'U) -> Q3
  (D'L, D'D) -> Q1

tickFlash :: G -> Bool
tickFlash g = (gTick g `mod` 60) <= 30

tickFlashFast :: G -> Bool
tickFlashFast g = (gTick g `mod` 20) <= 10

tickFlashSlow :: G -> Bool
tickFlashSlow g = ((15 + gTick g) `mod` 60) <= 30

data St = St
  { stGame :: G
  , stMouse :: (Int, Int)
  } deriving (Show, Eq)

initState :: St
initState = St
  { stGame = game1
  , stMouse = (0,0)
  }

activatePlayer :: P -> Map (Int, Int) U -> Map (Int, Int) U
activatePlayer p = fmap (\u -> u { uActive = uOwner u == p })

unitClassMovementRange :: C -> Int
unitClassMovementRange = \case
  C'Infantry -> 5
  C'Mech -> 4
  C'Recon -> 7
  C'Tank -> 5 

infantryTerrainCost :: T -> Maybe Int
infantryTerrainCost t = Just $ case t of
  T'Grass -> 1 
  T'Water -> 4
  T'Mountain -> 3
  T'Sand -> 1 
  T'Forest -> 2

mechTerrainCost :: T -> Maybe Int
mechTerrainCost t = Just $ case t of
  T'Grass -> 1 
  T'Water -> 4
  T'Mountain -> 3
  T'Sand -> 1 
  T'Forest -> 2

reconTerrainCost :: T -> Maybe Int
reconTerrainCost t = case t of
  T'Grass -> Just 1 
  T'Water -> Nothing
  T'Mountain -> Nothing
  T'Sand -> Just 1
  T'Forest -> Just 3

tankTerrainCost :: T -> Maybe Int
tankTerrainCost t = case t of
  T'Grass -> Just 1 
  T'Water -> Nothing
  T'Mountain -> Nothing
  T'Sand -> Just 2
  T'Forest -> Just 3

terrainCost :: C -> T -> Maybe Int
terrainCost c = case c of
  C'Infantry -> infantryTerrainCost
  C'Mech -> mechTerrainCost
  C'Recon -> reconTerrainCost
  C'Tank -> tankTerrainCost

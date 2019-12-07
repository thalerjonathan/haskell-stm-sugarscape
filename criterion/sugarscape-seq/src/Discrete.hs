{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Discrete 
  ( Discrete2dDimension
  , Discrete2dCoord
  , Discrete2dNeighbourhood
  , Discrete2dCell

  , Discrete2d (..)

  , EnvironmentWrapping (..)
  
  , createDiscrete2d

  , dimensionsDisc2d
  , dimensionsDisc2dM

  , allCells
  , allCellsWithCoords
  , allCellsWithCoordsM
  , updateCells
  , updateCellsM
  , updateCellsWithCoords
  , updateCellsWithCoordsM
  , updateCellAt
  , changeCellAt
  , changeCellAtM

  , cellsAt
  , cellAt
  , cellAtM
  , randomCell
  , randomCellM
  , randomCellWithinRect

  , neighbours
  , neighboursM
  , neighbourCells
  , neighbourCellsM

  , neighboursInNeumannDistance
  , neighboursInNeumannDistanceM
  , neighboursCellsInNeumannDistance
  , neighboursCellsInNeumannDistanceM

  , distanceManhattanDisc2d
  , distanceEuclideanDisc2d
  , neighbourhoodOf
  , neighbourhoodScale
  , wrapCells
  , neumann
  , moore
  , wrapNeighbourhood
  , wrapDisc2d
  , wrapDisc2dEnv
  ) where

import Data.Maybe

import qualified Data.IntMap.Strict as Map 
--import Control.Monad.Par
import Control.Monad.Random
import Control.Monad.State.Strict
import Control.DeepSeq
import GHC.Generics (Generic)

-- NOTE: used IArray before, would be very elegant but 2 problems
--       1. non-strict => leaking like hell
--       2. updates O(n) and copy whole array => awful performance

type Discrete2dDimension     = (Int, Int)
type Discrete2dCoord         = Discrete2dDimension
type Discrete2dNeighbourhood = [Discrete2dCoord]
type Discrete2dCell c        = (Discrete2dCoord, c)

data EnvironmentWrapping 
  = ClipToMax 
  | WrapHorizontal 
  | WrapVertical 
  | WrapBoth deriving (Show, Read, Eq, Generic, NFData)

data Discrete2d c = Discrete2d 
  { envDisc2dDims          :: !Discrete2dDimension
  , envDisc2dNeighbourhood :: !Discrete2dNeighbourhood
  , envDisc2dWrapping      :: !EnvironmentWrapping
  , envDisc2dCells         :: !(Map.IntMap (Discrete2dCell c))
  } deriving (Show, Read, Eq, Generic, NFData)

createDiscrete2d :: Discrete2dDimension
                 -> Discrete2dNeighbourhood
                 -> EnvironmentWrapping
                 -> [Discrete2dCell c]
                 -> Discrete2d c
createDiscrete2d d n w cs =
    Discrete2d {
      envDisc2dDims          = d
    , envDisc2dNeighbourhood = n
    , envDisc2dWrapping      = w
    , envDisc2dCells         = m
    }
  where
    ls = map (\(coord, c) -> (coordToIdx d coord, (coord, c))) cs
    m  = Map.fromList ls

coordToIdx :: Discrete2dDimension
           -> Discrete2dCoord 
           -> Int
coordToIdx (xDim, _) (x, y) = (y * (xDim - 1)) + x

dimensionsDisc2d :: Discrete2d c -> Discrete2dDimension
dimensionsDisc2d = envDisc2dDims

dimensionsDisc2dM :: MonadState (Discrete2d c) m
                  => m Discrete2dDimension
dimensionsDisc2dM = state (\e -> (envDisc2dDims e, e))

allCells :: Discrete2d c -> [c]
allCells e = map snd $ Map.elems $ envDisc2dCells e

allCellsWithCoords :: Discrete2d c -> [Discrete2dCell c]
allCellsWithCoords e = Map.elems $ envDisc2dCells e

allCellsWithCoordsM :: MonadState (Discrete2d c) m
                    => m [Discrete2dCell c]
allCellsWithCoordsM = state (\e -> (allCellsWithCoords e, e))

updateCellsM :: MonadState (Discrete2d c) m
             => (c -> c) 
             -> m ()
updateCellsM f = state (\e -> ((), updateCells f e))

updateCells :: (c -> c) -> Discrete2d c -> Discrete2d c
updateCells f e = e { envDisc2dCells = m' }
  where
    m  = envDisc2dCells e
    m' = Map.map forceStrict m
    -- m' = runPar $ do
    --   mIvar <- Map.traverseWithKey (\_ (coord, c) -> spawn (return (coord, f c))) m
    --   traverse Control.Monad.Par.get mIvar

    -- NOTE: by using this function and {-# LANGUAGE Strict #-}
    -- we were able to remove a serious memory-leak 
    -- The crucial part is that we make c' explicit which seems
    -- to force the result as well, just implementing as
    --forceStrict (coord, c) = (coord, f $! c) STILL LEAKS!
    forceStrict (coord, c) = (coord, c')
      where
        -- crucial bit is here
        c' = f c

updateCellsWithCoordsM :: MonadState (Discrete2d c) m
                       => (Discrete2dCell c -> c) 
                       -> m ()
updateCellsWithCoordsM f = state (\e -> ((), updateCellsWithCoords f e))

updateCellsWithCoords :: (Discrete2dCell c -> c) 
                      -> Discrete2d c 
                      -> Discrete2d c
updateCellsWithCoords f e = e { envDisc2dCells = m' }
  where
    m  = envDisc2dCells e
    m' = Map.map forceStrict m

    -- NOTE: by using this function and {-# LANGUAGE Strict #-}
    -- we were able to remove a serious memory-leak 
    -- The crucial part is that we make c' explicit which seems
    -- to force the result as well, just implementing as
    -- forceStrict (coord, c) = (coord, f $! c) STILL LEAKS!
    forceStrict cc@(coord, _) = (coord, c')
      where
        -- crucial bit is here
        c' = f cc

updateCellAt :: Discrete2dCoord 
             -> (c -> c) 
             -> Discrete2d c 
             -> Discrete2d c
updateCellAt coord f e = e { envDisc2dCells = m' }
  where
    m      = envDisc2dCells e
    d      = envDisc2dDims e
    idx    = coordToIdx d coord
    (_, c) = fromJust $ Map.lookup idx m -- should not fail if coord is within bounds
    c'     = f c
    m'     = Map.insert idx (coord, c') m

changeCellAt :: Discrete2dCoord -> c -> Discrete2d c -> Discrete2d c
changeCellAt coord c e = e { envDisc2dCells = m' }
  where
    m   = envDisc2dCells e
    d   = envDisc2dDims e
    idx = coordToIdx d coord
    m'  = Map.insert idx (coord, c) m

changeCellAtM :: MonadState (Discrete2d c) m
              => Discrete2dCoord 
              -> c 
              -> m ()
changeCellAtM coord c = state (\e -> ((), changeCellAt coord c e))

cellsAt :: [Discrete2dCoord] -> Discrete2d c -> [c]
cellsAt cs e = map (`cellAt` e) cs

cellAt :: Discrete2dCoord -> Discrete2d c -> c
cellAt coord e = c
  where
    m      = envDisc2dCells e
    d      = envDisc2dDims e
    idx    = coordToIdx d coord
    (_, c) = fromJust $ Map.lookup idx m

cellAtM :: MonadState (Discrete2d c) m
        => Discrete2dCoord 
        -> m c
cellAtM coord = state (\e -> (cellAt coord e, e))

randomCell :: MonadRandom m
           => Discrete2d c 
           -> m (c, Discrete2dCoord)
randomCell e = do
  let (maxX, maxY) = envDisc2dDims e

  randX <- getRandomR (0, maxX - 1) 
  randY <- getRandomR (0, maxY - 1)

  let randCoord = (randX, randY)
      randCell  = cellAt randCoord e

  return (randCell, randCoord)

randomCellM :: (MonadRandom m, MonadState (Discrete2d c) m)
            => m (c, Discrete2dCoord)
randomCellM = do
  (maxX, maxY) <- dimensionsDisc2dM
  randX        <- getRandomR (0, maxX - 1) 
  randY        <- getRandomR (0, maxY - 1)
  randCell     <- cellAtM (randX, randY)

  return (randCell, (randX, randY))

randomCellWithinRect :: MonadRandom m 
                     => Discrete2dCoord 
                     -> Int 
                     -> Discrete2d c
                     -> m (c, Discrete2dCoord)
randomCellWithinRect (x, y) r e =  do
  randX <- getRandomR (-r, r)
  randY <- getRandomR (-r, r)
  
  let randCoord = (x + randX, y + randY)
  let randCoordWrapped = wrapDisc2d (envDisc2dDims e) (envDisc2dWrapping e) randCoord
  let randCell = cellAt randCoordWrapped e

  return (randCell, randCoordWrapped)

-- NOTE: this function does only work for neumann-neighbourhood, it ignores
--       the environments neighbourhood. also it does not include the coord itself
neighboursInNeumannDistance :: Discrete2dCoord 
                            -> Int 
                            -> Bool 
                            -> Discrete2d c 
                            -> [Discrete2dCell c]
neighboursInNeumannDistance coord dist ic e = zip wrappedNs cells
  where
    n = neumann
    coordDeltas = foldr (\v acc -> acc ++ neighbourhoodScale n v) [] [1 .. dist]
    l = envDisc2dDims e
    w = envDisc2dWrapping e
    ns = neighbourhoodOf coord ic coordDeltas
    wrappedNs = wrapNeighbourhood l w ns
    cells = cellsAt wrappedNs e

neighboursInNeumannDistanceM :: MonadState (Discrete2d c) m
                             => Discrete2dCoord 
                             -> Int 
                             -> Bool 
                             -> m [Discrete2dCell c]
neighboursInNeumannDistanceM coord dist ic = 
  state (\e -> (neighboursInNeumannDistance coord dist ic e, e))

neighboursCellsInNeumannDistance :: Discrete2dCoord 
                                 -> Int 
                                 -> Bool 
                                 -> Discrete2d c 
                                 -> [c]
neighboursCellsInNeumannDistance coord dist ic e = 
  map snd (neighboursInNeumannDistance coord dist ic e)

neighboursCellsInNeumannDistanceM :: MonadState (Discrete2d c) m
                                  => Discrete2dCoord 
                                  -> Int 
                                  -> Bool 
                                  -> m [c]
neighboursCellsInNeumannDistanceM coord dist ic = 
  state (\e -> (neighboursCellsInNeumannDistance coord dist ic e, e))

neighbours :: Discrete2dCoord -> Bool -> Discrete2d c -> [Discrete2dCell c]
neighbours coord ic e = zip wrappedNs cells
  where
    n         = envDisc2dNeighbourhood e
    l         = envDisc2dDims e
    w         = envDisc2dWrapping e
    ns        = neighbourhoodOf coord ic n
    wrappedNs = wrapNeighbourhood l w ns
    cells     = cellsAt wrappedNs e

neighboursM :: MonadState (Discrete2d c) m
            => Discrete2dCoord 
            -> Bool 
            -> m [Discrete2dCell c]
neighboursM coord ic = state (\e -> (neighbours coord ic e, e))

neighbourCells :: Discrete2dCoord -> Bool -> Discrete2d c -> [c]
neighbourCells coord ic e = map snd (neighbours coord ic e)

neighbourCellsM :: MonadState (Discrete2d c) m
                => Discrete2dCoord 
                -> Bool 
                -> m [c]
neighbourCellsM coord ic = state (\e -> (neighbourCells coord ic e, e))
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
distanceManhattanDisc2d :: Discrete2dCoord -> Discrete2dCoord -> Int
distanceManhattanDisc2d (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

distanceEuclideanDisc2d :: Discrete2dCoord -> Discrete2dCoord -> Double
distanceEuclideanDisc2d (x1, y1) (x2, y2) = sqrt (xd*xd + yd*yd)
  where
    xd = fromRational $ toRational (x2 - x1)
    yd = fromRational $ toRational (y2 - y1)

neighbourhoodOf :: Discrete2dCoord 
                -> Bool 
                -> Discrete2dNeighbourhood 
                -> Discrete2dNeighbourhood
neighbourhoodOf coord@(x,y) includeCoord ns 
    | includeCoord = coord : ns'
    | otherwise = ns'
  where
    ns' = map (\(x', y') -> (x + x', y + y')) ns

neighbourhoodScale :: Discrete2dNeighbourhood -> Int -> Discrete2dNeighbourhood
neighbourhoodScale ns s = map (\(x,y) -> (x * s, y * s)) ns

wrapCells :: Discrete2dDimension 
          -> EnvironmentWrapping 
          -> Discrete2dNeighbourhood 
          -> Discrete2dNeighbourhood
wrapCells = wrapNeighbourhood

neumann :: Discrete2dNeighbourhood
neumann = [topDelta, leftDelta, rightDelta, bottomDelta]

moore :: Discrete2dNeighbourhood
moore = [ topLeftDelta,    topDelta,     topRightDelta,
          leftDelta,                     rightDelta,
          bottomLeftDelta, bottomDelta,  bottomRightDelta ]

wrapNeighbourhood :: Discrete2dDimension 
                  -> EnvironmentWrapping 
                  -> Discrete2dNeighbourhood 
                  -> Discrete2dNeighbourhood
wrapNeighbourhood l w = map (wrapDisc2d l w)

wrapDisc2dEnv :: Discrete2d c -> Discrete2dCoord -> Discrete2dCoord
wrapDisc2dEnv e = wrapDisc2d d w
  where
    d = envDisc2dDims e
    w = envDisc2dWrapping e

wrapDisc2d :: Discrete2dDimension 
           -> EnvironmentWrapping 
           -> Discrete2dCoord 
           -> Discrete2dCoord
wrapDisc2d (maxX, maxY) ClipToMax (x, y) 
  = (max 0 (min x (maxX - 1)), max 0 (min y (maxY - 1)))
wrapDisc2d l@(maxX, _) WrapHorizontal (x, y)
  | x < 0     = wrapDisc2d l WrapHorizontal (x + maxX, y)
  | x >= maxX = wrapDisc2d l WrapHorizontal (x - maxX, y)
  | otherwise = (x, y)
wrapDisc2d l@(_, maxY) WrapVertical (x, y)
  | y < 0     = wrapDisc2d l WrapVertical (x, y + maxY)
  | y >= maxY = wrapDisc2d l WrapVertical (x, y - maxY)
  | otherwise = (x, y)
wrapDisc2d l WrapBoth c 
  = wrapDisc2d l WrapHorizontal $ wrapDisc2d l WrapVertical  c

topLeftDelta :: Discrete2dCoord
topLeftDelta      = (-1, -1)
topDelta :: Discrete2dCoord
topDelta          = ( 0, -1)
topRightDelta :: Discrete2dCoord
topRightDelta     = ( 1, -1)
leftDelta :: Discrete2dCoord
leftDelta         = (-1,  0)
rightDelta :: Discrete2dCoord
rightDelta        = ( 1,  0)
bottomLeftDelta :: Discrete2dCoord
bottomLeftDelta   = (-1,  1)
bottomDelta :: Discrete2dCoord
bottomDelta       = ( 0,  1)
bottomRightDelta :: Discrete2dCoord
bottomRightDelta  = ( 1,  1)
-------------------------------------------------------------------------------
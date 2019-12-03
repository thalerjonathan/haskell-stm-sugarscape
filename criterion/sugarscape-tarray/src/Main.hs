module Main where

--import System.IO
import System.Random

import Control.Concurrent.STM
import Control.Concurrent.STM.Stats
import Control.Monad.Random
import Data.Time.Clock
import FRP.BearRiver
import qualified Criterion.Main as Crit

--import Discrete
import Environment
--import GlossRunner
import Init
import Model
import Simulation

durationSecs :: Double
durationSecs = 60

rngSeed :: Int
rngSeed = 42

envSize :: (Int, Int)
envSize = (50, 50)

main :: IO ()
main = do
    let t       = 100
        dt      = 1.0
        g       = mkStdGen rngSeed
        
    Crit.defaultMain [
        Crit.bgroup "sugarscape-tarray-cores"
        [ Crit.bench "500"  $ Crit.nfIO (initSim g t dt 500 False) ]
      , Crit.bgroup "sugarscape-tarray-agents"
        [ Crit.bench "500"  $ Crit.nfIO (initSim g t dt  500 True)
        , Crit.bench "1000" $ Crit.nfIO (initSim g t dt 1000 True)
        , Crit.bench "1500" $ Crit.nfIO (initSim g t dt 1500 True)
        , Crit.bench "2000" $ Crit.nfIO (initSim g t dt 2000 True)
        , Crit.bench "2500" $ Crit.nfIO (initSim g t dt 2500 True) ]
      ]
  where
    initSim g0 t dt ac rebirthFlag = do
      let envConc      = False  -- runs the environment agent concurrently DONT DO IT!
      let stmStatsFlag = False  -- collects STM statistics. WARNING: reduces performance!

      -- initial agents and environment
      let ret                = runRandT (createSugarScape ac envSize rebirthFlag) g0
      ((initAs, initEnv), g) <- atomically ret
      --envCells               <- atomically $ allCellsWithCoords initEnv

      -- initial model for Gloss = output of each simulation step to be rendered
      --let initOut                = (0, envCells, [])
      -- initial simulation state
      let (initAis, _)           = unzip initAs

      aidVar <- newTVarIO $ maximum initAis

      let sugCtx = SugContext {
          sugCtxEnv     = initEnv
        , sugCtxNextAid = aidVar
        }

      start <- getCurrentTime

      let initAs' = if envConc then (0, sugEnvironment) : initAs else initAs
          envAg   = if envConc then Nothing else Just sugEnvironment

      (dtVars, aoVars, g') <- spawnAgents initAs' g sugCtx stmStatsFlag
      -- initial simulation context
      let initSimCtx = mkSimContex dtVars aoVars 0 g' start 0 envAg

      simulateUntil dt t initSimCtx sugCtx stmStatsFlag []

-- main :: IO ()
-- main = do
--   hSetBuffering stdout LineBuffering

--   let stmStatsFlag = False  -- collects STM statistics. WARNING: reduces performance!
--       envConc      = False -- runs the environment agent concurrently
--       rebirthFlag  = True  -- an agent who dies will schedule to create a new random agent => keeps population (more or less) constant 
--       perfFile     = "50x50_2500_4_core_rebirth.txt"
--       glossOut     = False
--       rngSeed      = 42
--       dt           = 1.0 
--       agentCount   = 2500
--       envSize      = (50, 50)
--       -- initial RNG
--       g0           = mkStdGen rngSeed

--   -- initial agents and environment
--   let ret                = runRandT (createSugarScape agentCount envSize rebirthFlag) g0
--   ((initAs, initEnv), g) <- atomically ret
--   envCells               <- atomically $ allCellsWithCoords initEnv

--   -- initial model for Gloss = output of each simulation step to be rendered
--   let initOut                = (0, envCells, [])
--   -- initial simulation state
--   let (initAis, _)           = unzip initAs
 
--   aidVar <- newTVarIO $ maximum initAis

--   let sugCtx = SugContext {
--       sugCtxEnv     = initEnv
--     , sugCtxNextAid = aidVar
--     }

--   start <- getCurrentTime

--   let initAs' = if envConc then (0, sugEnvironment) : initAs else initAs
--       envAg   = if envConc then Nothing else Just sugEnvironment

--   (dtVars, aoVars, g') <- spawnAgents initAs' g sugCtx stmStatsFlag
--   -- initial simulation context
--   let initSimCtx = mkSimContex dtVars aoVars 0 g' start 0 envAg

--   if glossOut
--     then runWithGloss durationSecs dt initSimCtx sugCtx initOut stmStatsFlag perfFile
--     else simulate dt initSimCtx sugCtx stmStatsFlag perfFile

simulateUntil :: RandomGen g
              => DTime
              -> Time
              -> SimContext g
              -> SugContext
              -> Bool
              -> [SimStepOut]
              -> IO [SimStepOut]
simulateUntil dt tMax simCtx sugCtx stmStatsFlag acc = do
  (simCtx', simOut) <- simulationStep dt sugCtx simCtx stmStatsFlag

  let acc' = simOut : acc

  if simCtxTime simCtx' >= tMax
    then return acc'
    else simulateUntil dt tMax simCtx' sugCtx stmStatsFlag acc'

simulate :: RandomGen g
         => DTime
         -> SimContext g
         -> SugContext
         -> Bool
         -> String
         -> IO ()
simulate dt simCtx sugCtx stmStatsFlag perfFile = do
  (simCtx', (_, _, aos)) <- simulationStep dt sugCtx simCtx stmStatsFlag

  -- NOTE: need to print t otherwise lazy evaluation would omit all computation
  print $ length aos
  
  ret <- checkTime durationSecs simCtx' perfFile
  if ret 
    then when stmStatsFlag dumpSTMStats
    else simulate dt simCtx' sugCtx stmStatsFlag perfFile
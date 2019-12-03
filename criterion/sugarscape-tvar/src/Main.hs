module Main where

--import System.IO
import System.Random

import Control.Concurrent.STM
import Control.Concurrent
import Control.Concurrent.STM.Stats
import Control.Monad.Random
import Data.Time.Clock
import FRP.BearRiver
import qualified Criterion.Main as Crit

--import GlossRunner
import Environment
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
    let t  = 100
        dt = 1.0
        g  = mkStdGen rngSeed
    
    cores <- getNumCapabilities

    Crit.defaultMain [
        Crit.bgroup "sugarscape-tvar-cores"
        [ Crit.bench ("500:"  ++ show cores) $ Crit.nfIO (initSim g t dt 500 False) ]
      , Crit.bgroup "sugarscape-tvar-agents"
        [ Crit.bench ("500:"  ++ show cores) $ Crit.nfIO (initSim g t dt  500 True)
        , Crit.bench ("1000:" ++ show cores) $ Crit.nfIO (initSim g t dt 1000 True)
        , Crit.bench ("1500:" ++ show cores) $ Crit.nfIO (initSim g t dt 1500 True)
        , Crit.bench ("2000:" ++ show cores) $ Crit.nfIO (initSim g t dt 2000 True)
        , Crit.bench ("2500:" ++ show cores) $ Crit.nfIO (initSim g t dt 2500 True) ]
      ]
  where
    initSim g0 t dt ac rebirthFlag = do
      let envConc      = False  -- runs the environment agent concurrently DONT DO IT!
      let stmStatsFlag = False  -- collects STM statistics. WARNING: reduces performance!

      let -- initial agents and environment
          ((initAs, initEnv), g) = runRand (createSugarScape ac envSize rebirthFlag) g0
          -- initial model for Gloss = output of each simulation step to be rendered
          -- initOut                = (0, initEnv, [])
          -- initial simulation state
          (initAis, _)           = unzip initAs

      envVar <- newTVarIO initEnv
      aidVar <- newTVarIO $ maximum initAis

      let sugCtx = SugContext {
        sugCtxEnv     = envVar
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
--       rebirthFlag  = True -- an agent who dies will schedule to create a new random agent => keeps population (more or less) constant 
--       perfFile     = "50x50_2500_4_core_rebirth.txt"
--       glossOut     = False
--       rngSeed      = 42
--       dt           = 1.0     -- this model has discrete time-semantics with a step-with of 1.0 which is relevant for the aging of the agents
--       agentCount   = 2500
--       envSize      = (50, 50)

--       -- initial RNG
--       g0           = mkStdGen rngSeed
--       -- initial agents and environment
--       ((initAs, initEnv), g) = runRand (createSugarScape agentCount envSize rebirthFlag) g0
--       -- initial model for Gloss = output of each simulation step to be rendered
--       initOut                = (0, initEnv, [])
--       -- initial simulation state
--       (initAis, _)           = unzip initAs

--   envVar <- newTVarIO initEnv
--   aidVar <- newTVarIO $ maximum initAis

--   let sugCtx = SugContext {
--       sugCtxEnv     = envVar
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
  (simCtx', (t, _, aos)) <- simulationStep dt sugCtx simCtx stmStatsFlag

  -- NOTE: need to print t otherwise lazy evaluation would omit all computation
  putStrLn $ "t = " ++ show t ++ " agents = " ++ show (length aos)
  
  ret <- checkTime durationSecs simCtx' perfFile
  if ret 
    then when stmStatsFlag dumpSTMStats
    else simulate dt simCtx' sugCtx stmStatsFlag perfFile
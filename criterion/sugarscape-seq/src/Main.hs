module Main where

--import System.IO
import System.Random

import Control.Monad.Random
import Data.Time.Clock
import FRP.BearRiver
import qualified Criterion.Main as Crit

import AgentMonad
--import GlossRunner
import Init
import Simulation

durationSecs :: Double
durationSecs = 60
rngSeed :: Int
rngSeed = 42

envSize :: (Int, Int)
envSize = (50, 50)

main :: IO ()
main = do
    let steps   = 100
        dt      = 1
        
    Crit.defaultMain [
        Crit.bgroup "sugarscape-seq-singlecore"
        [ Crit.bench "500"  $ Crit.nfIO (initSim steps dt 500 False) ]
      , Crit.bgroup "sugarscape-seq-agents"
        [ Crit.bench "500"  $ Crit.nfIO (initSim steps dt  500 True)
        , Crit.bench "1000" $ Crit.nfIO (initSim steps dt 1000 True)
        , Crit.bench "1500" $ Crit.nfIO (initSim steps dt 1500 True)
        , Crit.bench "2000" $ Crit.nfIO (initSim steps dt 2000 True)
        , Crit.bench "2500" $ Crit.nfIO (initSim steps dt 2500 True) ]
      ]
  where
    initSim steps dt ac rebirthFlag = do
          -- initial RNG
      let (g0, shuffleRng) = split $ mkStdGen rngSeed
          -- initial agents and environment
          ((initAs, initEnv), g) = runRand (createSugarScape ac envSize rebirthFlag) g0
          -- initial simulation state
          (initAis, initSfs) = unzip initAs
      
      start <- getCurrentTime
      
      let initSimState = mkSimState (simStepSF initAis initSfs shuffleRng) 
                                    (mkAbsState $ maximum initAis) 
                                    initEnv g start 0
      
      simulateUntil dt steps initSimState []

-- main :: IO ()
-- main = do
--   hSetBuffering stdout LineBuffering

--   let glossOut    = False
--       rebirthFlag = True -- an agent who dies will schedule to create a new random agent => keeps population (more or less) constant 
--       perfFile    = "50x50_2000_rebirth.txt"
--       rngSeed     = 42
--       dt          = 1.0
--       agentCount  = 2000
--       envSize     = (50, 50)

--       -- initial RNG
--       (g0, shuffleRng) = split $ mkStdGen rngSeed
--       -- initial agents and environment
--       ((initAs, initEnv), g) = runRand (createSugarScape agentCount envSize rebirthFlag) g0
--       -- initial simulation state
--       (initAis, initSfs) = unzip initAs

--   start <- getCurrentTime

--   let initSimState = mkSimState (simStepSF initAis initSfs shuffleRng) (mkAbsState $ maximum initAis) initEnv g start 0

--   if glossOut
--     then runWithGloss durationSecs dt initSimState (0, initEnv, []) perfFile
--     else simulate dt initSimState perfFile

simulateUntil :: RandomGen g
              => DTime
              -> Int
              -> SimulationState g
              -> [SimStepOut]
              -> IO [SimStepOut]
simulateUntil dt steps ss acc = do
  (ss', simOut) <- simulationStep dt ss

  let acc' = simOut : acc

  if simSteps ss' >= steps
    then return acc'
    else simulateUntil dt steps ss' acc'

simulate :: RandomGen g
         => DTime
         -> SimulationState g
         -> String 
         -> IO ()
simulate dt ss perfFile = do
  (ss', (t, _, aos)) <- simulationStep dt ss

  -- NOTE: need to print t otherwise lazy evaluation would omit all computation
  --putStrLn $ "t = " ++ show t ++ " agents = " ++ show (length aos)
  print $ length aos
  
  ret <- checkTime durationSecs ss' perfFile
  if ret 
    then return ()
    else simulate dt ss' perfFile
module Simulation 
  (
    SimStepOut
  , SimContext (..)

  , simulationStep
  , spawnAgents

  , mkSimContex
  , checkTime
  ) where

import           Data.Maybe
import           System.Random

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.Stats
import           Control.Monad.Random
import           Control.Monad.Reader
import           Data.Time.Clock
import           FRP.BearRiver

import           Common
import           Discrete
import           Model
import           Renderer

type SimStepOut = (Time, [Discrete2dCell SugEnvCell], [AgentObservable SugAgentObservable])

data SimContext g = SimContext
  { simCtxDtVars :: [MVar DTime]
  , simCtxAoVars :: [MVar (AgentId, SugAgentOut g)]
  , simCtxTime   :: Time
  , simCtxRng    :: g
  , simCtxStart  :: UTCTime
  , simCtxSteps  :: Int
  , simCtxEnvAg  :: Maybe (SugAgent g)
  }

simulationStep :: RandomGen g
               => DTime
               -> SugContext
               -> SimContext g
               -> Bool
               -> IO (SimContext g, SimStepOut)
simulationStep dt sugCtx simCtx stmStatsFlag = do
  let dtVars = simCtxDtVars simCtx
      aoVars = simCtxAoVars simCtx
      t      = simCtxTime simCtx
      g      = simCtxRng simCtx
      start  = simCtxStart simCtx
      steps  = simCtxSteps simCtx
      envAg  = simCtxEnvAg simCtx

  -- tell all threads to continue with the corresponding DTime
  mapM_ (`putMVar` dt) dtVars
  -- wait for results
  aos <- mapM takeMVar aoVars
  -- read the latest environment cells
  envCells <- atomically $ allCellsWithCoords $ sugCtxEnv sugCtx

  let newAs = concatMap (\(_, ao) -> sugAoNew ao) aos
      obs   = foldr (\(aid, ao) acc -> 
        if isObservable ao 
          then (aid, fromJust $ sugAoObservable ao) : acc  
          else acc) [] aos

  (newDtVars, newAoVars, g') <- spawnAgents newAs g sugCtx stmStatsFlag

  let (dtVars', aoVars') = foldr (\((_, ao), dv, aov) acc@(accDtVars, accAoVars) -> 
        if isDead ao 
          then acc 
          else (dv : accDtVars, aov : accAoVars))  ([], []) (zip3 aos dtVars aoVars)

  let dtVars'' = dtVars' ++ newDtVars
      aoVars'' = aoVars' ++ newAoVars
      t'      = t + dt

  simCtx' <- if isJust envAg
              then do
                -- putStrLn "Running Environment non-concurrently"
                (_, envAg', g'') <- runAgentStep (fromJust envAg) dt g' sugCtx stmStatsFlag
                return $ mkSimContex dtVars'' aoVars'' t' g'' start (steps + 1) (Just envAg')
              else return $ mkSimContex dtVars'' aoVars'' t' g' start (steps + 1) envAg

  return (simCtx', (t, envCells, obs))

spawnAgents :: RandomGen g
            => [(AgentId, SugAgent g)]
            -> g
            -> SugContext
            -> Bool
            -> IO ([MVar DTime], [MVar (AgentId, SugAgentOut g)], g)
spawnAgents [] g0 _ _ = return ([], [], g0)
spawnAgents ((aid, a) : as) g0 sugCtx stmStatsFlag = do
  let (g', g'') = split g0

  dtVar <- newEmptyMVar 
  aoVar <- createAgentThread dtVar g' sugCtx aid a stmStatsFlag

  (dtVars, aoVars, g) <- spawnAgents as g'' sugCtx stmStatsFlag

  return (dtVar : dtVars, aoVar : aoVars, g)

runAgentStep :: RandomGen g 
             => SugAgent g
             ->  DTime
             -> g
             -> SugContext
             -> Bool
             -> IO (SugAgentOut g, SugAgent g, g)
runAgentStep sf dt rng sugCtx stmStatsFlag = do
  -- compute next step
  let sfDtReader  = unMSF sf SugAgentIn
      sfCtxReader = runReaderT sfDtReader dt
      sfRand      = runReaderT sfCtxReader sugCtx
      sfSTM       = runRandT sfRand rng
  ((ao, sf'), rng') <- if stmStatsFlag then trackSTMConf stmConf "SugarScape" sfSTM else atomically sfSTM 
  -- NOTE: running STM with stats results in considerable lower performance the more STM actions are run concurrently
  return (ao, sf', rng')

createAgentThread :: RandomGen g 
                  => MVar DTime
                  -> g
                  -> SugContext
                  -> AgentId
                  -> SugAgent g
                  -> Bool
                  -> IO (MVar (AgentId, SugAgentOut g))
createAgentThread dtVar rng0 sugCtx aid a stmStatsFlag = do
    -- create the var where the result will be posted to
    retVar <- newEmptyMVar
    _ <- forkIO $ agentThread a rng0 retVar
    return retVar
  where
    agentThread :: RandomGen g 
                => SugAgent g
                -> g
                -> MVar (AgentId, SugAgentOut g)
                -> IO ()
    agentThread sf rng retVar = do
      -- wait for next dt to compute next step
      dt <- takeMVar dtVar

      (ao, sf', rng') <- runAgentStep sf dt rng sugCtx stmStatsFlag
      
      -- post result to main thread
      putMVar retVar (aid, ao)

      if isDead ao 
        then return ()
        else agentThread sf' rng' retVar

mkSimContex :: [MVar DTime]
            -> [MVar (AgentId, SugAgentOut g)]
            -> Time
            -> g
            -> UTCTime
            -> Int
            -> Maybe (SugAgent g)
            -> SimContext g
mkSimContex dtVars aoVars t g start steps envAg = SimContext { 
    simCtxDtVars = dtVars
  , simCtxAoVars = aoVars
  , simCtxTime   = t
  , simCtxRng    = g
  , simCtxStart  = start
  , simCtxSteps  = steps
  , simCtxEnvAg  = envAg
  }

stmConf :: TrackSTMConf
stmConf = defaultTrackSTMConf {
    tryThreshold   = Nothing
  , globalTheshold = Nothing
  }

checkTime :: RandomGen g
          => Double
          -> SimContext g
          -> String
          -> IO Bool
checkTime durSecs simCtx fileName = do
  nowT <- getCurrentTime

  let start = simCtxStart simCtx
  let dtStart = realToFrac $ diffUTCTime nowT start

  if dtStart > durSecs
    then (do 
      let steps      = simCtxSteps simCtx
          stepsRatio = (fromIntegral steps / durSecs) :: Double

      appendFile fileName $ show steps ++ " steps after " ++ show durSecs ++ " sec. is a ratio of " ++ show stepsRatio ++ "\n"
      -- putStrLn $ show steps ++ " steps after " ++ show durSecs ++ " sec. is a ratio of " ++ show stepsRatio
    
      return True)
    else return False
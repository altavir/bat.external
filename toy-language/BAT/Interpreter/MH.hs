{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Implement simple Metropolis-Hasting sampler
module BAT.Interpreter.MH (
    ModelMCMC(..)
  , Proposal
  , makeMH
  , runMH
  ) where

import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Reader
import           Data.Functor.Identity
import           Data.Foldable
import qualified Data.Map as Map
import           Data.Map   (Map)
import qualified Data.Set as Set
import           Data.Set   (Set)

import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

import qualified Statistics.Distribution             as Dist
import qualified Statistics.Distribution.Exponential as Dist
import qualified Statistics.Distribution.Poisson     as Dist


import BAT.Algorithms
import BAT.AST
import BAT.Types
import BAT.Typecheck


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Model prepared for MCMC sampling
data ModelMCMC = ModelMCMC
  { modelLogP      :: Map Var Value -> Double
    -- ^ Evaluate log-probability for model
  , modelProposals :: Map Var Proposal
    -- ^ Generators for proposals for models
  }

data Stmt
  = StmtRnd Var (Distrib (Expr Var))
  | StmtObs     (Distrib (Expr Var)) Value
  | StmtDet Var (Expr Var)
  deriving (Show)

data Proposal = PropNormal Double
              | PropUI     Int
              deriving (Show)


----------------------------------------------------------------
-- Generation of model
----------------------------------------------------------------

-- | Convert declarative description of a model to a form suitable for
--   running as MCMC
makeMH :: MonadBAT m => Program -> m ModelMCMC
makeMH prog@(Program dataDecl model obs) = do
  -- Program validation
  validateProgram prog
  typecheck prog
  -- Evaluate all data prior to evaluation and substitute them to
  -- model
  dat <- reduceDataDecls prog
  let subst v = case v `Map.lookup` dat of
        Just val -> Lit val
        Nothing  -> V v
      substDecl = \case
        Random        d -> Random        (fmap (>>= subst) d)
        Deterministic d -> Deterministic (d >>= subst)
      model' = fmap (fmap substDecl) model
  ----------------------------------------
  -- 1. Toposort model
  let modelVars = Map.keysSet model'
      varDeps   = Set.intersection modelVars . Set.fromList . concat . toList . fmap toList <$> model'
      Just topoVars = reverse <$> toposort modelVars varDeps
  -- 2. Convert model to statements for subsequent execution
  let stmts = [ case decl of
                  Deterministic e -> StmtDet v e
                  Random        d -> case v `Map.lookup` obs of
                    Nothing  -> StmtRnd v d
                    Just val -> StmtObs d val
              | v <- topoVars
              , let Just (Typed _ decl) = Map.lookup v model'
              ]
  -- 3. Create function for evaluation of proposal
  let evalStep (!vars,!logp) = \case
        StmtObs d val -> (vars, logp + evalDistr vars d val)
        StmtRnd v d   -> (vars, logp + evalDistr vars d (vars Map.! v))
        StmtDet v expr -> let val = evalExpr vars expr
                          in (Map.insert v val vars, logp)
  -- 4. Automatically guess proposals
  return ModelMCMC
    { modelLogP      = \vars -> snd $ foldl' evalStep (vars,0) stmts
    , modelProposals = Map.fromList
        [ (v, case d of
                Flat          -> PropNormal 1
                Poisson{}     -> PropUI 2
                Exponential{} -> PropNormal 1
          )
        | (v, Typed _ (Random d)) <- Map.toList model'
        , v `Map.notMember` obs
        ]
    }

evalExpr :: Map Var Value -> Expr Var -> Value
evalExpr vars
  = runIdentity
  . flip runReaderT vars
  . unSeqEval
  . evaluateExpr

evalDistr :: Map Var Value -> Distrib (Expr Var) -> Value -> Double
evalDistr vars distrib genVal =
  case evalExpr vars <$> distrib of
    Flat -> error "FIXME: Flat is not implemented"
    Poisson (ValI i) -> evalPoisson (fromIntegral i)
    Poisson (ValD x) -> evalPoisson x
    Poisson _        -> error "FIXME: internal error"
    Exponential (ValI i) -> evalExponential (fromIntegral i)
    Exponential (ValD x) -> evalExponential x
    Exponential _        -> error "FIXME: internal error"
  where
    evalPoisson lam = case genVal of
      ValI n -> case Dist.poissonE lam of
                  Nothing -> (-1) / 0
                  Just d  -> Dist.logProbability d n
      _      -> error "FIXME: invalid type for generated value"
    evalExponential lam = case genVal of
      ValD x -> case Dist.exponentialE lam of
                  Nothing -> (-1) / 0
                  Just d  -> Dist.logDensity d x
      _      -> error "FIXME: invalid type for generated value"



----------------------------------------------------------------
-- Running chain
----------------------------------------------------------------

-- | Run Markov chain simulation for given number of steps. Current
--   implementation is totally inefficient but who cares anyway?
runMH :: ModelMCMC -> Map Var Value -> MWC.Seed -> Int -> [Map Var Value]
runMH model x0 seed nIter = runST $ do
  gen <- MWC.restore seed
  let loop 0 _ _    = return []
      loop n x logP = do
        props <- traverse (flip propose gen) (modelProposals model)
        let y     = Map.unionWith add x props
            logPy = modelLogP model y
        p <- MWC.uniform gen
        let (x',logP') | log p < logPy - logP = (y,logPy)
                       | otherwise            = (x,logP)
        --
        xs <- loop (n-1) x' logP'
        return (x' : xs)
  --
  loop nIter x0 (modelLogP model x0)


propose :: Proposal -> MWC.Gen s -> ST s Value
propose (PropNormal σ) gen = ValD <$> MWC.normal 0 σ gen
propose (PropUI n)     gen = ValI <$> MWC.uniformR (-n,n) gen

add :: Value -> Value -> Value
add (ValI a) (ValI b) = ValI (a+b)
add (ValI a) (ValD b) = ValD (fromIntegral a+b)
add (ValD a) (ValI b) = ValD (a+fromIntegral b)
add (ValD a) (ValD b) = ValD (a+b)
add _ _ = error "Internal error"

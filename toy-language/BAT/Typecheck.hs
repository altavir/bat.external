{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
-- |
-- Type checking and evaluation of expressions in BAT programs. They
-- don't neatly separate since we want to allow types to depend on
-- values in data section of program (e.g. array dimensions) and
-- therefore we have to interleave reduction of expressions and type
-- checking
module BAT.Typecheck (
    -- * Validation and type checking
    validateProgram
  , typecheck
    -- * Evaluation
  , MonadBATEval(..)
  , RecEval(..)
  , SeqEval(..)
  , evaluateExpr
  , reduceDataDecls
  ) where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Foldable
import Data.Monoid
import qualified Data.Map as Map
import           Data.Map   (Map)
import qualified Data.Set as Set
import           Data.Set   (Set)

import BAT.AST
import BAT.Algorithms
import BAT.Types


----------------------------------------------------------------
-- Validation
----------------------------------------------------------------

-- | Check that there're no unbound variables.
validateProgram :: MonadBAT m => Program -> m ()
validateProgram (Program pdata mdl obs) = do
  -- Check that we don't have unbound variables in data and model sections
  (traverse_ . traverse_ . traverse_) (varIsBound dataVars) pdata
  (traverse_ . traverse_ . traverse_) (varIsBound (modelVars<>dataVars)) mdl
  -- We don't redeclare variable in model secion
  unless (Set.null $ Set.intersection dataVars modelVars) $
    batError UNSPECIFIED
  -- All observed variables are random variables in model section
  forM_ (Map.keys obs) $ \v -> case v `Map.lookup` mdl of
    Just (Typed _ Random{})        -> return ()
    Just (Typed _ Deterministic{}) -> batError UNSPECIFIED
    Nothing                        -> batError UNSPECIFIED
  -- Check that we don't have loops in the data section
  do let vars = Map.keysSet pdata
         deps = Set.fromList . (toList <=< toList) <$> pdata
     case toposort vars deps of
       Nothing -> batError DependencyLoop
       Just _  -> return ()
  -- Check that we don't have loops in the model section
  do let vars = Map.keysSet mdl
         deps = Set.intersection vars . Set.fromList . (toList <=< toList) <$> mdl
     case toposort vars deps of
       Nothing -> batError DependencyLoop
       Just _  -> return ()
  where
    dataVars  = Map.keysSet pdata
    modelVars = Map.keysSet mdl

varIsBound :: MonadBAT m => Set Var -> Var -> m ()
varIsBound vs v
  = unless (v `Set.member` vs) $ batError (UndefinedVar v)


----------------------------------------------------------------
-- Type checking
----------------------------------------------------------------

-- | Check type correctness of model. There're following open design
--   questions:
--
--   1) We only check type correctness of program and let interpreter
--      do type promotion. Alternative is to resolve type of
--      overloaded operator and insert explicit type cast. Choice is
--      likely to be determined by details of Julia code generation.
typecheck :: MonadBAT m => Program -> m ()
typecheck (Program pdata mdl obs) = do
  -- Language is explicitly typed so we can collect types for all
  -- declarations easily
  let tyMap = mconcat
        [ typeDecl <$> mdl
        , typeDecl <$> pdata
        ]
  forM_ mdl $ \case
    -- Typecheck determenistic expressions
    Typed ty (Deterministic expr) -> do
      ty' <- typecheckExpr tyMap expr
      unless (ty == ty') (batError TypeMismatch)
    -- Typecheck random variables
    Typed Real (Random Flat) -> return ()
    Typed Int  (Random (Poisson e)) ->
      typecheckExpr tyMap e >>= \case
        Real -> return ()
        Int  -> return ()
        _    -> batError TypeMismatch
    Typed Real (Random (Exponential e)) ->
      typecheckExpr tyMap e >>= \case
        Real -> return ()
        Int  -> return ()
        _    -> batError TypeMismatch
    _ -> batError TypeMismatch

-- | Typecheck expression given types for all variables
typecheckExpr :: MonadBAT m => Map Var Ty -> Expr Var -> m Ty
typecheckExpr tyMap = go
  where
    go = \case
      V x       -> case x `Map.lookup` tyMap of
                     Just ty -> return ty
                     Nothing -> batError (UndefinedVar x)
      Lit v     -> return $ case v of
        ValI _ -> Int
        ValD _ -> Real
        ValB _ -> Bool
      e1 :+: e2 -> promoteNum (go e1) (go e2)
      e1 :-: e2 -> promoteNum (go e1) (go e2)
      e1 :*: e2 -> promoteNum (go e1) (go e2)
      e1 :/: e2 -> do ty <- promoteNum (go e1) (go e2)
                      case ty of Int  -> return Real
                                 Real -> return Real
                                 Bool -> batError TypeMismatch -- unreachable
      Neg e     -> go e

promoteNum :: MonadBAT m => m Ty -> m Ty -> m Ty
promoteNum mty1 mty2 = do
  ty1 <- mty1
  ty2 <- mty2
  case (ty1,ty2) of
    (Int  , Int)  -> return Int
    (Int  , Real) -> return Real
    (Real , Int)  -> return Real
    (Real , Real) -> return Real
    _             -> batError TypeMismatch



----------------------------------------------------------------
-- Reduction
----------------------------------------------------------------

-- | Type class for monad for performing evaluation of expressions. We
--   have two types of such monad: one which perform evaluation
--   recursively and one that assumes that all variables in
--   expressions are already have clue bound to them.
class MonadBAT m => MonadBATEval m where
  evalVar :: Var -> m Value

-- | Monad for recursive evaluation of expressions. It's assumed that
--   dependencies form DAG. Any loop will lead to nontermination.
newtype RecEval m a = RecEval
  { unRecEval :: ReaderT (Map Var (Expr Var)) (StateT (Map Var Value) m) a }
  deriving (Functor,Applicative,Monad,MonadIO)

instance MonadTrans RecEval where
  lift = RecEval . lift . lift
  
instance MonadBAT m => MonadBAT (RecEval m) where
  batError = RecEval . lift . lift . batError

instance MonadBAT m => MonadBATEval (RecEval m) where
  evalVar v = RecEval $
    gets (Map.lookup v) >>= \case
      Just val -> return val
      Nothing  -> asks (Map.lookup v) >>= \case
        Just expr -> unRecEval (evaluateExpr expr)
        Nothing   -> lift $ lift $ batError UNSPECIFIED

-- | Monad for sequential evaluation. All dependencies should be
--   evaluated in advance
newtype SeqEval m a = SeqEval
  { unSeqEval :: ReaderT (Map Var Value) m a }
  deriving (Functor,Applicative,Monad,MonadIO)

instance MonadTrans SeqEval where
  lift = SeqEval . lift

instance MonadBAT m => MonadBAT (SeqEval m) where
  batError = SeqEval . lift . batError

instance MonadBAT m => MonadBATEval (SeqEval m) where
  evalVar v = SeqEval $
    asks (Map.lookup v) >>= \case
      Just val -> return val
      Nothing  -> lift $ batError UNSPECIFIED

-- | Reduce data declarations to normal form. Note that we don't need
--   to type check them. All error would be found during reduction.
reduceDataDecls :: MonadBAT m => Program -> m (Map Var Value)
reduceDataDecls (Program pdata mdl obs)
  -- = undefined
  = flip execStateT Map.empty
  $ flip runReaderT (valueDecl <$> pdata)
  $ unRecEval
  $ (traverse . traverse) evaluateExpr pdata

-- | Reduce expression to value
evaluateExpr :: MonadBATEval m => Expr Var -> m Value
evaluateExpr = eval
  where
    eval = \case
      V x       -> evalVar x
      Lit v     -> return v
      e1 :+: e2 -> liftNumOp2 (+) (+) (eval e1) (eval e2)
      e1 :-: e2 -> liftNumOp2 (-) (-) (eval e1) (eval e2)
      e1 :*: e2 -> liftNumOp2 (*) (*) (eval e1) (eval e2)
      e1 :/: e2 -> liftNumOp2D (/) (eval e1) (eval e2)
      Neg e     -> liftNumOp  negate negate (eval e)



-- type MonadReduce = ReaderT (Set Var) (StateT (Map Var Value) MonadBAT)

-- notLoop :: Var -> MonadReduce ()
-- notLoop v = do
--   stack <- ask
--   when (v `Set.member` stack) $ batError (UndefinedVar v)

-- push :: Var -> MonadReduce a -> MonadReduce a
-- push v = local (Set.insert v)

-- reduceDVar :: Map Var (Ty, Expr Var) -> Var -> MonadReduce Value
-- reduceDVar dataExpr v = do
--   notLoop v
--   gets (Map.lookup v) >>= \case
--     Just val -> return val
--     Nothing  -> case v `Map.lookup` dataExpr of
--       Nothing    -> batError (UndefinedVar v)
--       Just (_,e) -> do val <- push v $ reduceExpr dataExpr e
--                        modify' (Map.insert v val)
--                        return val

-- reduceExpr :: Map Var (Ty, Expr Var) -> Expr Var -> MonadReduce Value
-- reduceExpr dataExpr = go
--   where
--     go = \case
--       V x       -> reduceDVar dataExpr x
--       Lit v     -> return v
--       e1 :+: e2 -> liftNumOp2 (+) (+) (go e1) (go e2)
--       e1 :-: e2 -> liftNumOp2 (-) (-) (go e1) (go e2)
--       e1 :*: e2 -> liftNumOp2 (*) (*) (go e1) (go e2)
--       -- FIXME: deal with division
--       e1 :/: e2 -> liftNumOp2 undefined (/) (go e1) (go e2)
--       Neg e     -> liftNumOp  negate negate (go e)

liftNumOp
  :: MonadBATEval m
  => (Int -> Int)
  -> (Double -> Double)
  -> m Value
  -> m Value
liftNumOp funI funD mx = do
  x <- mx
  case x of
    ValI i -> return $ ValI $ funI i
    ValD d -> return $ ValD $ funD d
    ValB _ -> batError TypeMismatch

liftNumOp2
  :: MonadBATEval m
  => (Int    -> Int    -> Int)
  -> (Double -> Double -> Double)
  -> m Value
  -> m Value
  -> m Value
liftNumOp2 funI funD mx my = do
  x <- mx
  y <- my
  case (x,y) of
    (ValI a, ValI b) -> return $ ValI $ funI a b
    (ValI a, ValD b) -> return $ ValD $ funD (fromIntegral a) b
    (ValD a, ValI b) -> return $ ValD $ funD a (fromIntegral b)
    (ValD a, ValD b) -> return $ ValD $ funD a b
    _                -> batError TypeMismatch

liftNumOp2D
  :: MonadBATEval m
  => (Double -> Double -> Double)
  -> m Value
  -> m Value
  -> m Value
liftNumOp2D funD mx my = do
  x <- mx
  y <- my
  case (x,y) of
    (ValI a, ValI b) -> return $ ValD $ funD (fromIntegral a) (fromIntegral b)
    (ValI a, ValD b) -> return $ ValD $ funD (fromIntegral a) b
    (ValD a, ValI b) -> return $ ValD $ funD a (fromIntegral b)
    (ValD a, ValD b) -> return $ ValD $ funD a b
    _                -> batError TypeMismatch





-- ----------------------------------------------------------------
-- -- Evaluations
-- ----------------------------------------------------------------



-- ----------------------------------------------------------------
-- -- Helpers
-- ----------------------------------------------------------------

-- reduceExpr' :: Map Var Value -> Expr Var -> Value
-- reduceExpr' values = go
--   where
--     go = \case
--       V x       -> values Map.! x
--       Lit v     -> v
--       e1 :+: e2 -> liftNumOp2' (+) (+) (go e1) (go e2)
--       e1 :-: e2 -> liftNumOp2' (-) (-) (go e1) (go e2)
--       e1 :*: e2 -> liftNumOp2' (*) (*) (go e1) (go e2)
--       -- FIXME: deal with division
--       e1 :/: e2 -> liftNumOp2' undefined (/) (go e1) (go e2)
--       Neg e     -> liftNumOp'  negate negate (go e)

-- liftNumOp'
--   :: (Int -> Int)
--   -> (Double -> Double)
--   -> Value
--   -> Value
-- liftNumOp' funI funD x =
--   case x of
--     ValI i -> ValI $ funI i
--     ValD d -> ValD $ funD d
--     ValB _ -> error "TypeMismatch"

-- liftNumOp2'
--   :: (Int    -> Int    -> Int)
--   -> (Double -> Double -> Double)
--   -> Value
--   -> Value
--   -> Value
-- liftNumOp2' funI funD x y =
--   case (x,y) of
--     (ValI a, ValI b) -> ValI $ funI a b
--     (ValI a, ValD b) -> ValD $ funD (fromIntegral a) b
--     (ValD a, ValI b) -> ValD $ funD a (fromIntegral b)
--     (ValD a, ValD b) -> ValD $ funD a b
--     _                -> error "TypeMismatch"

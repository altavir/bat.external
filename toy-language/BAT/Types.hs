{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
-- |
module BAT.Types where

import Control.Exception
import Data.Functor.Identity

import BAT.AST

----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Error during evaluation of program
data BATError
  = UndefinedVar Var
    -- ^ Program contains undefined variable
  | DependencyLoop
    -- ^ There's dependency loop in the program
  | TypeMismatch
    -- ^ Type error
  | UNSPECIFIED
    -- ^ Placeholder error. Each use should be replaced with more
    --   specific error
  deriving (Show,Eq)

instance Exception BATError

class Monad m => MonadBAT m where
  batError :: BATError -> m a

instance (e ~ BATError) => MonadBAT (Either e) where
  batError = Left

instance MonadBAT Identity where
  batError = throw

instance MonadBAT IO where
  batError = throwIO

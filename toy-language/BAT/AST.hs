{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
--
module BAT.AST where

import Control.Applicative
import Control.Monad
import Data.String
import qualified Data.Map as Map
import           Data.Map   (Map)

----------------------------------------------------------------
-- Model specifications as AST
----------------------------------------------------------------

-- | Type of variable. At the moment only scalar types are supported.
data Ty
  = Int
  | Real
  | Bool
  deriving (Show,Eq,Ord)

-- | Possible values in the language. We only have doubles so far
data Value
  = ValI !Int
  | ValD !Double
  | ValB !Bool
  deriving (Show,Eq,Ord)

-- | Variable names are encoded as strings
newtype Var = Var String
  deriving (Show,Eq,Ord,IsString)


-- | Complete description of probabilistic program
data Program = Program
  { progData     :: Map Var (Typed (Expr Var))
    -- ^ Data declarations they are evaluated before we starting
    --   inference
  , progModel    :: Map Var (Typed (Decl Var))
    -- ^ Model declarations.
  , progObserved :: Map Var Value
    -- ^ Observed random values. At the moment restricted to values.
  }
  deriving (Show)

-- | Value with type attached.
data Typed a = Typed
  { typeDecl  :: Ty
  , valueDecl :: a
  }
  deriving (Show,Eq,Ord, Functor,Foldable,Traversable)

-- | Right side of variable definition. It could be either random
--   or a deterministic.
data Decl  a
  = Random        (Distrib (Expr a))
  | Deterministic (Expr a)
  deriving (Show,Eq,Ord, Functor,Foldable,Traversable)

-- | Distributions. For now they're hardcoded
data Distrib a
  = Flat
  | Poisson     !a
  | Exponential !a
  deriving (Show,Eq,Ord, Functor,Foldable,Traversable)

-- | AST for expressions. Parameter type is type for variables and
--   monad instance encodes variable substitution.
data Expr a
  = V    !a
  | Lit  !Value
  | Expr a :+: Expr a
  | Expr a :-: Expr a
  | Expr a :*: Expr a
  | Expr a :/: Expr a
  | Neg (Expr a)
  deriving (Show,Eq,Ord, Functor,Foldable,Traversable)

instance Applicative Expr where
  pure = V
  (<*>) = ap
instance Monad Expr where
  return = V
  x >>= f = case x of
    V a     -> f a
    Lit l   -> Lit l
    a :+: b -> (a >>= f) :+: (b >>= f)
    a :-: b -> (a >>= f) :-: (b >>= f)
    a :*: b -> (a >>= f) :*: (b >>= f)
    a :/: b -> (a >>= f) :/: (b >>= f)
    Neg a   -> Neg (a >>= f)

instance Num (Expr a) where
  fromInteger = Lit . ValI . fromInteger
  (+) = (:+:)
  (-) = (:-:)
  (*) = (:*:)
  negate = Neg
  abs    = error "NOT IMPLEMENTED (abs)"
  signum = error "NOT IMPLEMENTED (signum)"

instance Fractional (Expr a) where
  fromRational = Lit . ValD . fromRational
  (/) = (:/:)

instance IsString a => IsString (Expr a) where
  fromString = V . fromString


----------------------------------------------------------------
-- Sugar for AST construction
----------------------------------------------------------------

(.:) :: Var -> Ty -> Typed Var
(.:) = flip Typed
infixl 6 .:

(.=) :: Typed Var -> Expr Var -> (Var, Typed (Decl Var))
Typed ty v .= e = (v, Typed ty (Deterministic e))
infixl 5 .=

(.~) :: Typed Var -> Distrib (Expr Var) -> (Var, Typed (Decl Var))
Typed ty v .~ d = (v, Typed ty (Random d))
infixl 5 .~

model :: [(Var, Typed (Decl Var))] -> Map Var (Typed (Decl Var))
model = Map.fromList

obs :: [(Var, Value)] -> Map Var Value
obs = Map.fromList

(.==) :: Var -> Value -> (Var, Value)
v .== e = (v, e)
infixl 5 .==

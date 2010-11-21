{-# LANGUAGE TransformListComp #-}

module Numeric.LinearProgramming.Easy
    ( -- * The Simplex monad
      Simplex
    , runSimplex
    , maximize
    , minimize

      -- ** Values & variables
    , SimplexValue
    , SimplexVar
    , newVar
    , newVars

      -- ** Constraints
    , (.<=)
    , (.==)
    , (.>=)
    ) where

import Control.Arrow
import Control.Monad.State
import Data.Function
import Data.List
import Text.Printf
import Numeric.LinearProgramming

type SimplexValue = SimplexExpr

-- | The simplex expression representation
data SimplexExpr
    = Vars [ (Double, Int) ]
    | Val  Double
  deriving Eq

instance Show SimplexExpr where
    show (Val v)     = show v
    show (Vars vars) = intercalate " + " [ count ++ "v" ++ show i
                                         | (d,i) <- vars
                                         , let count = if d == 1 then "" else show d ++ " * "
                                         ]

instance Num SimplexExpr where

    (Vars a) + (Vars b) = Vars (a++b)
    a        + b        = impossibleExpression a "+" b

    (Val  a) * (Vars b) = Vars (map (first (*a)) b)
    (Vars a) * (Val  b) = Vars (map (first (*b)) a)
    (Val  a) * (Val  b) = Val (a*b)
    a        * b        = impossibleExpression a "*" b

    negate (Val a)  = Val (negate a)
    negate (Vars a) = Vars (map (first negate) a)

    abs (Val a)     = Val (abs a)
    abs (Vars a)    = Vars (map (first abs) a)

    signum (Val a)  = Val (signum a)
    signum a        = impossibleExpression' "signum" a

    fromInteger     = Val . fromInteger

instance Fractional SimplexExpr where
    fromRational a      = Val (fromRational a)
    (Vars a) / (Val  b) = Vars (map (first (/b)) a)
    (Val  a) / (Val  b) = Val (a/b)
    a        / b        = nonlinearExpression a "/" b

-- Error messages
impossibleExpression  :: (Show a, Show b) => a -> String -> b -> c
impossibleExpression' :: (Show b) => String -> b -> c
nonlinearExpression   :: (Show a, Show b) => a -> String -> b -> c
impossibleExpression a e b = error $ printf "Impossible expression: %s %s %s" (show a) e (show b)
impossibleExpression'  e b = error $ printf "Impossible expression: %s %s" e (show b)
nonlinearExpression  a e b = error $ printf "Nonlinear expression: %s %s %s" (show a) e (show b)


type SimplexVar = SimplexExpr

newtype Simplex a = Simplex { unsimplex :: State Int a }

instance Monad Simplex where
    return a          = Simplex (return a)
    (Simplex a) >>= b = Simplex (a >>= unsimplex . b)

runSimplex :: Simplex a -> a
runSimplex (Simplex s) = evalState s 0


type SimplexConstraints = Bound [(Double, Int)]


-- | Turn simplex expressions into a list with their value and id.
-- Assures that every variable occurs exactly once.
constraints :: SimplexExpr -> [(Double,Int)]
constraints (Val v)    = impossibleExpression' "constraints" (Val v)
constraints (Vars c) =
    [ (sum d,head i)
    | (d,i) <- c
    , then group by i
    ]


(.<=) :: SimplexExpr -> SimplexExpr -> SimplexConstraints
(Vars a) .<= (Val  b) = a :<=: b
(Vars a) .<= (Vars b) = constraints (Vars a - Vars b) :<=: 0
(Val  a) .<= (Vars b) = b :=>: a
(Val  a) .<= (Val  b) = error $ printf "Empty constraint: %f <= $f" a b
infix 4 .<=

(.>=) :: SimplexExpr -> SimplexExpr -> SimplexConstraints
(Vars a) .>= (Val  b) = a :=>: b
(Vars a) .>= (Vars b) = constraints (Vars a - Vars b) :=>: 0
(Val  a) .>= (Vars b) = b :<=: a
(Val  a) .>= (Val  b) = error $ printf "Empty constraint: %f >= $f" a b
infix 4 .>=

(.==) :: SimplexExpr -> SimplexExpr -> SimplexConstraints
(Vars a) .== (Val  b) = a :==: b
(Vars a) .== (Vars b) = constraints (Vars a - Vars b) :==: 0
(Val  a) .== (Vars b) = b :==: a
(Val  a) .== (Val  b) = error $ printf "Empty constraint: %f == $f" a b
infix 4 .==


-- | Create a new variable
newVar :: Simplex SimplexVar
newVar = Simplex $ do
    i <- gets (+1)
    put i
    return $ Vars [(1,i)]

-- | Create a list of @n@ new variables.
newVars :: Int -> Simplex [SimplexVar]
newVars n = replicateM n newVar


minimize :: SimplexExpr -> [SimplexConstraints] -> Simplex Solution
minimize ex c = return $ simplex opt (Sparse c) []
  where
    opt :: Optimization
    opt = Minimize $ map fst sorted

    sorted = sortBy (compare `on` snd) (constraints ex)

maximize :: SimplexExpr -> [SimplexConstraints] -> Simplex Solution
maximize ex c = return $ simplex opt (Sparse c) []
  where
    opt :: Optimization
    opt = Maximize $ map fst sorted

    sorted = sortBy (compare `on` snd) (constraints ex)

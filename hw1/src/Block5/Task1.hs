{-# LANGUAGE ScopedTypeVariables #-}

module Block5.Task1
  ( ArithmeticResult
  , Expr (..)
  , ArithmeticError (..)
  , evalExpr
  ) where

-- | Type alias for Either ArithmeticError a.
type ArithmeticResult a = Either ArithmeticError a

-- | Structure representing arithmetic expressions.
-- Constructors:
--   * Const represents single value
--   * Add is addition of two expressions
--   * Sub is subtraction of two expressions
--   * Mul is multiplication of two expressions
--   * Div is division of two expressions
--   * Pow is power of two expressions
data Expr a
  = Const a
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Div (Expr a) (Expr a)
  | Pow (Expr a) (Expr a)
  deriving (Show, Eq)

-- | Structure representing error that can occur while evaluating expressions.
-- Constructors:
--   * DivisionByZero occurs when expression is divided by zero
--   * NegativePow occurs when expression is powered to negative number.
data ArithmeticError
  = DivisionByZero
  | NegativePow
  deriving (Show, Eq)

-- | Evaluate expression and return either result of evaluation or error.
evalExpr :: forall a . (Integral a) => Expr a -> ArithmeticResult a
evalExpr (Const value) = Right value
evalExpr (Add lhs rhs) = evalImpl (\x y -> Right $ x + y) lhs rhs
evalExpr (Sub lhs rhs) = evalImpl (\x y -> Right $ x - y) lhs rhs
evalExpr (Mul lhs rhs) = evalImpl (\x y -> Right $ x * y) lhs rhs
evalExpr (Div lhs rhs) = evalImpl divImpl lhs rhs
  where
    divImpl x y
      | y == 0 = Left DivisionByZero
      | otherwise = Right $ x `div` y
evalExpr (Pow lhs rhs) = evalImpl powImpl lhs rhs
  where
    powImpl x y
      | y < 0 = Left NegativePow
      | otherwise = Right $ x ^ y

evalImpl :: (Integral a) => (a -> a -> ArithmeticResult a) -> Expr a -> Expr a -> ArithmeticResult a
evalImpl op l r =
  evalExpr l >>= \x -> 
  evalExpr r >>= \y -> 
  x `op` y

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE  FlexibleInstances #-}


module Calc where
import           ExprT
import           Parser
import           StackVM
import           Data.Maybe

-- ExprT.Add and StackVM.Add is uses to prevent compiler ambiguity as both modules expose same type constructor
-- FlexibleInstances allow things instance Expr StackVM.Program where without having typevariables a1,a2, like in T (a1,a2 ..)
-- it also allows us to make instances of type synonyms like StackVM.Program

eval :: ExprT -> Integer
eval (Lit a        ) = a
eval (ExprT.Mul x y) = eval x * eval y
eval (ExprT.Add x y) = eval x + eval y

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit ExprT.Add ExprT.Mul

class Expr a where
    lit :: Integer -> a
    add :: a -> a ->  a
    mul :: a -> a ->  a

instance Expr ExprT where
    lit = Lit
    add = ExprT.Add
    mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit x | x < 1     = False
          | otherwise = True
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq,Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving(Eq,Show)

instance Expr Mod7 where
    lit x = Mod7 (x `mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
    mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)

-- parseExp does the heavy lifting and turns string to Expr a
-- we write intances of Expr typeclass and can check the validity using testExp

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3*-4) + 5"

testInteger :: Maybe Integer
testInteger = testExp

testBool :: Maybe Bool
testBool = testExp

testMM :: Maybe MinMax
testMM = testExp

testSat :: Maybe Mod7
testSat = testExp

instance Expr StackVM.Program where
    lit x = [StackVM.PushI x]
    add x y = x ++ y ++ [StackVM.Add]
    mul x y = x ++ y ++ [StackVM.Mul]

testProg :: Maybe StackVM.Program
testProg = testExp

compile :: String -> Maybe Program
compile = parseExp lit add mul

-- execute the complete program using stackVM which takes a Program and executes it ..
-- it returns Either ErrorString or value of type StackVal (Integer,Bool or Void in case of empty programs)
-- program is a list of Stack Expressions(StackExp) hence empty list(default) for Nothing case in fromMaybe

runProgram :: String -> Either String StackVal
runProgram = stackVM . fromMaybe [] . compile



module Expreval (
) where

import Data.Char

data Operator = Addition
              | Multiplication
    deriving (Show)

data ExpressionTree = OperatorNode Operator ExpressionTree ExpressionTree
                    | ValueNode Integer
    deriving (Show)

eval :: ExpressionTree -> Integer
eval (ValueNode a) = a
eval (OperatorNode Addition a b) = eval a + eval b
eval (OperatorNode Multiplication a b) = eval a * eval b

data ExpressionToken = OperatorToken Operator
                     | ValueToken Integer
    deriving (Show)

prec :: ExpressionToken -> Integer
prec (ValueToken _) = 0
prec (OperatorToken Multiplication) = 1
prec (OperatorToken Addition) = 2


tokenize :: String -> [ExpressionToken]
tokenize s
    | null s = []
    | isDigit $ head s =
        let (h, t) = span isDigit s
        in ValueToken (read h) : (tokenize t)
    | isSpace $ head s = tokenize (dropWhile isSpace s)
    | '+' == head s = OperatorToken Addition : (tokenize $ tail s)
    | '*' == head s = OperatorToken Multiplication : (tokenize $ tail s)

intopost :: [ExpressionToken] -> [ExpressionToken]
intopost ts = intopost' ts [] []

intopost' :: [ExpressionToken] -> [ExpressionToken] -> [ExpressionToken] -> [ExpressionToken]
intopost' [] [] rs = rs
intopost' [] os rs = rs ++ os
intopost' (x:xs) os rs = case x of
    ValueToken _ -> intopost' xs os (rs ++ [x])
    OperatorToken Multiplication -> intopost' xs (x : os) rs
    OperatorToken Addition -> intopost' xs oo rr where
        c t = prec t < prec x
        (r, o) = span c os
        oo = x : o
        rr = rs ++ r

posttotree :: [ExpressionToken] -> ExpressionTree
posttotree [] = ValueNode 0
posttotree xs = posttotree' xs [] where
    posttotree' [] stk = head stk
    posttotree' (x' : xs') stk = case x' of
        ValueToken v -> posttotree' xs' (ValueNode v : stk)
        OperatorToken o ->
            let (fst:snd:rst) = stk
            in posttotree' xs' (OperatorNode o fst snd : rst)

split :: (Char -> Bool) -> String -> [String]
split p s = case dropWhile p s of
                "" -> []
                s' -> w : split p s''
                      where (w, s'') = break p s'

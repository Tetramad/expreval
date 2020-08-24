module Expreval (
    expreval
) where

import Data.Char

data Operator = Addition
              | Subtraction
              | Multiplication
              | Division
    deriving (Show)

data OpenClose = Open
               | Close
    deriving (Show)

data Token = Operator Operator
           | Number Integer
           | Parenthesis OpenClose
    deriving (Show)

precedence :: Token -> Integer
precedence (Parenthesis _) = 0
precedence (Number _) = 1
precedence (Operator Multiplication) = 3
precedence (Operator Division) = 3
precedence (Operator Addition) = 4
precedence (Operator Subtraction) = 4

toNumber :: String -> Token
toNumber s = Number (read s)

tokenize :: String -> [Token]
tokenize [] = []
tokenize s
    | isDigit $ head s =
        let (ds, rs) = span isDigit s
        in toNumber ds : tokenize rs
    | isSpace $ head s = tokenize $ dropWhile isSpace s
    | '+' == head s =
        let rs = tail s
        in Operator Addition : tokenize rs
    | '-' == head s =
        let rs = tail s
        in Operator Subtraction : tokenize rs
    | '*' == head s =
        let rs = tail s
        in Operator Multiplication : tokenize rs
    | '/' == head s =
        let rs = tail s
        in Operator Division : tokenize rs
    | '(' == head s =
        let rs = tail s
        in Parenthesis Open : tokenize rs
    | ')' == head s =
        let rs = tail s
        in Parenthesis Close : tokenize rs

shuntingYard :: [Token] -> [Token]
shuntingYard [] = []
shuntingYard xs = shuntingYard' xs [] [] where
    shuntingYard' [] [] zs = zs
    shuntingYard' [] ys zs = zs ++ ys
    shuntingYard' (Parenthesis Open : xs') ys zs = shuntingYard' xs' (Parenthesis Open:ys) zs
    shuntingYard' (Parenthesis Close : xs') ys zs = shuntingYard' xs' ys' zs' where
        popCondition (Parenthesis _) = False
        popCondition _ = True
        (zs'', ys'') = span popCondition ys
        ys' = tail ys''
        zs' = zs ++ zs''
    shuntingYard' (x':xs') ys zs = shuntingYard' xs' (x':ys') zs' where
        popCondition (Parenthesis _) = False
        popCondition a = precedence a <= precedence x'
        (zs'', ys'') = span popCondition ys
        ys' = ys''
        zs' = zs ++ zs''

data ExpressionTree = BinaryNode Token ExpressionTree ExpressionTree
                    | ValueNode Token
    deriving (Show)

treefy :: [Token] -> ExpressionTree
treefy ts = treefy' ts [] where
    treefy' [] [] = ValueNode (Number 0)
    treefy' [] (r:_) = r
    treefy' (x':xs') rs = case x' of
        Number _ -> treefy' xs' (ValueNode x' : rs)
        Operator _ -> let (fst:snd:rst) = rs
                      in treefy' xs' (BinaryNode x' snd fst : rst)

eval :: ExpressionTree -> Integer
eval (ValueNode (Number x)) = x
eval (BinaryNode (Operator Addition) a b) = eval a + eval b
eval (BinaryNode (Operator Subtraction) a b) = eval a - eval b
eval (BinaryNode (Operator Multiplication) a b) = eval a * eval b
eval (BinaryNode (Operator Division) a b) = div (eval a) (eval b)

expreval :: String -> Integer
expreval = eval . treefy . shuntingYard . tokenize

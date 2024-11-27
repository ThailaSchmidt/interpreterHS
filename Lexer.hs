module Lexer where 

import Data.Char

--define o tipo de dados das expressões
data Expr = BTrue 
          | BFalse 
          | Num Int 
          | Add Expr Expr 
          | Sub Expr Expr --
          | Mul Expr Expr--
          | And Expr Expr 
          | Or Expr Expr --
          | Not Expr --
          | Eq Expr Expr
          | MrEq Expr Expr --
          | If Expr Expr Expr 
          | Var String 
          | Lam String Ty Expr 
          | App Expr Expr
          | Cons Expr Expr -- construtor cons
          | IsNil Expr    -- testa se é lista vazia
          | Nil           -- null
          | Head Expr     -- acessa o primeiro elemento
          | Tail Expr     -- acessa a cauda
          deriving (Eq)

-- mostra as expr bonitinhas
instance Show Expr where
    show BTrue = "True"
    show BFalse = "False"
    show (Num n) = show n
    show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (Sub e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
    show (Mul e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
    show (And e1 e2) = "(" ++ show e1 ++ " && " ++ show e2 ++ ")"
    show (Or e1 e2) = "(" ++ show e1 ++ " || " ++ show e2 ++ ")"
    show (Not e) = "!" ++ show e
    show (Eq e1 e2) = "(" ++ show e1 ++ " == " ++ show e2 ++ ")"
    show (MrEq e1 e2) = "(" ++ show e1 ++ " >= " ++ show e2 ++ ")"
    show (If e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
    show (Var v) = v
    show (Lam v t e) = "(\\" ++ v ++ " -> " ++ show e ++ ")"
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Cons e1 e2) = show e1 ++ " , " ++ show e2 
    show Nil = "[]"
    show (IsNil e) = show e
    show (Head e) = show e
    show (Tail e) = show e

--tipo de dados
data Ty = TBool 
        | TNum 
        | TFun Ty Ty 
        | TList Ty  --tipo para listas
        deriving (Show, Eq)

data Token = TokenTrue
           | TokenFalse 
           | TokenNum Int 
           | TokenAdd 
           | TokenSub
           | TokenMul
           | TokenAnd 
           | TokenOr
           | TokenNot
           | TokenEq
           | TokenMrEq
           | TokenIf
           | TokenThen
           | TokenElse 
           | TokenVar String
           | TokenLam 
           | TokenArrow
           --Listas
           | TokenCons 
           | TokenIsNil 
           | TokenNil
           | TokenHead 
           | TokenTail 
           deriving Show

--converte uma string em uma lista de tokens
lexer :: String -> [Token]
lexer [] = [] 
lexer ('+':cs) = TokenAdd : lexer cs 
lexer ('-':cs) = TokenSub : lexer cs 
lexer ('*':cs) = TokenMul : lexer cs 
lexer ('\\':cs) = TokenLam : lexer cs 
lexer ('=':'=':cs) = TokenEq : lexer cs 
lexer ('>':'=':cs) = TokenMrEq : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs 
lexer (c:cs) 
   | isSpace c = lexer cs 
   | isAlpha c = lexerKW (c:cs) 
   | isDigit c = lexerNum (c:cs)

--processa valores e os converte em tokens 
lexerNum :: String -> [Token]
lexerNum cs = case span isDigit cs of 
                (num, rest) -> TokenNum (read num) : lexer rest

--processa palavras-chave e variáveis
lexerKW :: String -> [Token]
lexerKW cs = case span isAlpha cs of 
               ("true", rest) -> TokenTrue : lexer rest 
               ("false", rest) -> TokenFalse : lexer rest 
               ("and", rest) -> TokenAnd : lexer rest 
               ("or", rest) -> TokenOr : lexer rest
               ("not", rest) -> TokenNot : lexer rest
               ("if", rest) -> TokenIf : lexer rest 
               ("then", rest) -> TokenThen : lexer rest 
               ("else", rest) -> TokenElse : lexer rest 
               --Listas
               ("cons", rest) -> TokenCons : lexer rest
               ("isnil", rest) -> TokenIsNil : lexer rest
               ("nil", rest) -> TokenNil : lexer rest 
               ("head", rest) -> TokenHead : lexer rest
               ("tail", rest) -> TokenTail : lexer rest
               (var, rest) -> TokenVar var : lexer rest


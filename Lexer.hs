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
          | List [Expr] --listas
          deriving (Show, Eq)

--tipo de dados para os tipos
data Ty = TBool 
        | TNum 
        | TFun Ty Ty 
        | TList Ty    -- Tipo para listas, que leva o tipo dos elementos
        deriving (Show, Eq)

--tipos de dados para os tokens
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
           | TokenLBracket  -- Token [
           | TokenRBracket  -- Token ]
           | TokenComma     -- Token para a vírgula que separa os elementos da lista
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
lexer ('[':cs) = TokenLBracket : lexer cs  -- L
lexer (']':cs) = TokenRBracket : lexer cs  -- L
lexer (',':cs) = TokenComma : lexer cs    -- L
lexer (c:cs) 
   | isSpace c = lexer cs 
   | isAlpha c = lexerKW (c:cs) 
   | isDigit c = lexerNum (c:cs)

--processa números (inteiros) e os converte em tokens TokenNum
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
               (var, rest) -> TokenVar var : lexer rest


module Main where 

import Lexer 
import Parser
import Interpreter
import TypeChecker

expr1 :: Expr
expr1 = Add (Num 5) (Num 3)  -- 5 + 3 = 8

-- Exemplo de expressão: Lógica And
expr2 :: Expr
expr2 = And BTrue (Num 1)  -- Deve resultar em Num 1

-- Exemplo de expressão: Condicional (If)
expr3 :: Expr
expr3 = If BTrue (Num 10) (Num 20)  -- Deve resultar em Num 10

-- Exemplo de expressão: Função Lambda e Aplicação
expr4 :: Expr
expr4 = App (Lam "x" TNum (Add (Var "x") (Num 2))) (Num 3)  -- Deve resultar em Num 5
-- Exemplo de expressão: Lista (Cons e IsNil)
expr5 :: Expr
expr5 = Cons (Num 1) (Cons (Num 2) (Cons (Num 3) Nil))  -- Lista: [1, 2, 3]

-- Exemplo de uso de Head e Tail
expr6 :: Expr
expr6 = Head expr5  -- Esperado: Num 1, que é o primeiro elemento da lista

expr7 :: Expr
expr7 = Tail expr5  -- Esperado: Cons (Num 2) (Cons (Num 3) Nil), que é a cauda da lista

expr8 :: Expr
expr8 = IsNil expr5

expr9 :: Expr
expr9 = IsNil Nil  -- Esperado: True, pois Nil representa uma lista vazia

main = do
    -- Testes para as expressões
    print (eval expr1)  -- Esperado: Num 8
    print (eval expr2)  -- Esperado: Num 1
    print (eval expr3)  -- Esperado: Num 10
    print (eval expr4)  -- Esperado: Num 5
    putStrLn "Teste Listas:" 
    putStrLn "Print Lista:" 
    print (eval expr5)  -- Esperado: Cons (Num 1) (Cons (Num 2) (Cons (Num 3) Nil))
    putStrLn "Print Head:" 
    print (eval expr6)  -- Esperado: Num 1
    putStrLn "Print Tail:" 
    print (eval expr7)  -- Esperado: Cons (Num 2) (Cons (Num 3) Nil)
    putStrLn "Print é vazia?"
    print (eval expr8)  -- Esperado: False
    putStrLn "Print IsNil (lista vazia):"
    print (eval expr9)  -- Esperado: True

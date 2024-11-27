module Main where 

import Lexer 
import Parser
import Interpreter
import TypeChecker

expr1 :: Expr
expr1 = Add (Num 5) (Num 3)

expr2 :: Expr
expr2 = And BTrue (Num 1)  

expr3 :: Expr
expr3 = If BTrue (Num 10) (Num 20)  

-- Exemplo de expressão: Função Lambda e Aplicação
expr4 :: Expr
expr4 = App (Lam "x" TNum (Add (Var "x") (Num 2))) (Num 3)  -- Deve resultar em Num 5

-- Exemplo de Lista 
expr5 :: Expr
expr5 = Cons (Num 5) (Cons (Num 6) (Cons (Num 8) Nil)) 

-- Exemplo de uso de Head, Tail e IsNil
expr6 :: Expr
expr6 = Head expr5  

expr7 :: Expr
expr7 = Tail expr5  

expr8 :: Expr
expr8 = IsNil expr5

expr9 :: Expr
expr9 = IsNil Nil 

main = do
    print (eval expr1) 
    print (eval expr2)  
    print (eval expr3)  
    print (eval expr4)  
    putStrLn "Teste Listas:" 
    putStrLn "Print Lista:" 
    print (eval expr5)  
    putStrLn "Print Head:" 
    print (eval expr6) 
    putStrLn "Print Tail:" 
    print (eval expr7) 
    putStrLn "Print é vazia?"
    print (eval expr8) 
    putStrLn "Print IsNil (lista vazia):"
    print (eval expr9) 


--eval (Head (Cons (Num 1) (Cons (Num 2) Nil)))
--eval (Cons (Add (Num 1) (Num 1)) (Cons (Num 2) Nil))
--eval (IsNil Nil)
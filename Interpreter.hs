module Interpreter where 

import Lexer 

-- verifica se uma expressão é um valor
isValue :: Expr -> Bool
isValue BTrue = True 
isValue BFalse = True 
isValue (Num _) = True 
isValue (Lam _ _ _) = True
isValue (Cons _ _) = True  -- Lista não vazia é um valor 
isValue _ = False 

-- Função: subst
-------------------- Substitui uma variável (x) por uma nova expressão (n) no corpo da função (b)
-- Parâmetros: Variável (Parâmetro formal), 
--             Expressão (Parâmetro atual), 
--             Corpo da função
-- Retorno: Corpo da função substituído
---------------------------------------------------       
subst :: String -> Expr -> Expr -> Expr
subst x n b@(Var v) = if v == x then 
                        n 
                      else 
                        b 
subst x n (Add e1 e2) = (Add (subst x n e1) (subst x n e2))
subst x n (Sub e1 e2) = (Sub (subst x n e1) (subst x n e2))
subst x n (Mul e1 e2) = (Mul (subst x n e1) (subst x n e2))
subst x n (And e1 e2) = (And (subst x n e1) (subst x n e2))
subst x n (Or e1 e2) = (Or (subst x n e1) (subst x n e2))
subst x n (Not e1) = (Not (subst x n e1))
subst x n (Eq e1 e2) = (Eq (subst x n e1) (subst x n e2))
subst x n (MrEq e1 e2) = (MrEq (subst x n e1) (subst x n e2))
subst x n (If e e1 e2) = (If (subst x n e) (subst x n e1) (subst x n e2))
subst x n (Lam v t b) = (Lam v t (subst x n b))
subst x n (App e1 e2) = (App (subst x n e1) (subst x n e2))
subst x n (Cons e1 e2) = Cons (subst x n e1) (subst x n e2)
subst x n (IsNil e) = IsNil (subst x n e)
subst x n (Head e) = Head (subst x n e)
subst x n (Tail e) = Tail (subst x n e)
subst _ _ e = e

--Step realiza um único passo de avaliação em uma expressão
step :: Expr -> Expr 
step (Add (Num n1) (Num n2)) = Num (n1 + n2)
step (Add (Num nv) e2) = let e2' = step e2 
                           in Add (Num nv) e2' 
step (Add e1 e2) = Add (step e1) e2 

step (Sub (Num n1) (Num n2)) = Num (n1 - n2)
step (Sub (Num nv) e2) = let e2' = step e2 
                           in Sub (Num nv) e2' 
step (Sub e1 e2) = Sub (step e1) e2 

step (Mul (Num n1) (Num n2)) = Num (n1 * n2)
step (Mul (Num nv) e2) = let e2' = step e2 
                          in Mul (Num nv) e2' 
step (Mul e1 e2) = Mul (step e1) e2

step (And BFalse e) = BFalse 
step (And BTrue e) = e 
step (And e1 e2) = And (step e1) e2 

step (Or BFalse e) = step e
step (Or BTrue _) = BTrue
step (Or e1 e2) = Or (step e1) e2

step (Not BFalse) = BTrue
step (Not BTrue) = BFalse
step (Not e) = Not (step e)

step (Eq e1 e2) | isValue e1 && isValue e2 = if (e1 == e2) then 
                                               BTrue 
                                             else 
                                               BFalse
                | isValue e1 = Eq e1 (step e2)
                | otherwise = Eq (step e1) e2

step (MrEq (Num n1) (Num n2)) = if n1 >= n2 then
                                  BTrue 
                                else 
                                  BFalse
step (MrEq e1 e2) | isValue e1 = MrEq e1 (step e2)
                  | otherwise = MrEq (step e1) e2

step (If BTrue e1 e2) = e1 
step (If BFalse e1 e2) = e2 
step (If e e1 e2) = If (step e) e1 e2 

step (App (Lam v t b) e) | isValue e = subst v e b 
                         | otherwise = (App (Lam v t b) (step e))
step (App e1 e2) = App (step e1) e2 

-- LISTAS
step (Cons e1 e2) = Cons (step e1) (step e2)

step (IsNil Nil) = BTrue
step (IsNil (Cons _ _)) = BFalse
step (IsNil _) = error "Não é uma lista"               -- Se não for nem Nil nem Cons, é um erro ou valor não válido

step (Head (Cons e1 _)) = e1  -- Retorna o primeiro elemento
step (Head _) = error "Head aplicado a algo que não é uma lista"

step (Tail (Cons _ e2)) = e2  -- Acessa a cauda
step (Tail (IsNil _)) = error "Cannot apply tail to an empty list"


--eval avalia completamente a expressão
eval :: Expr -> Expr 
eval e | isValue e = e 
       | otherwise = eval (step e)

module TypeChecker where 

import Lexer 

type Ctx = [(String, Ty)]

-- Função principal que realiza a verificação de tipo de uma expressão
typeof :: Ctx -> Expr -> Maybe Ty 
typeof _ (Num _) = Just TNum 
typeof _ BTrue = Just TBool
typeof _ BFalse = Just TBool
--verifica se ambos os operandos são do tipo TNum e retorna TNum
typeof ctx (Add e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                           (Just TNum, Just TNum) -> Just TNum
                           _ -> Nothing 
typeof ctx (Sub e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                           (Just TNum, Just TNum) -> Just TNum
                           _ -> Nothing                            
typeof ctx (Mul e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                           (Just TNum, Just TNum) -> Just TNum
                           _ -> Nothing 
--verifica se os operandos são do tipo TBool
typeof ctx (And e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                           (Just TBool, Just TBool) -> Just TBool
                           _ -> Nothing
typeof ctx (Or e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                           (Just TBool, Just TBool) -> Just TBool
                           _ -> Nothing
typeof ctx (Not e1) = case (typeof ctx e1) of 
                           (Just TBool) -> Just TBool
                           _ -> Nothing
--os dois operandos devem ser do mesmo tipo e o resultado será TBool
typeof ctx (Eq e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                          (Just t1, Just t2) | t1 == t2 -> Just TBool 
                                             | otherwise -> Nothing 
                          _ -> Nothing
--espera dois operandos do tipo TNum e retorna TBool
typeof ctx (MrEq e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                          (Just TNum, Just TNum) -> Just TBool
                          _ -> Nothing
--o primeiro operando deve ser do tipo TBool e os dois lados do if devem ter o mesmo tipo
typeof ctx (If e e1 e2) = case typeof ctx e of 
                            Just TBool -> case (typeof ctx e1, typeof ctx e2) of 
                                            (Just t1, Just t2) | t1 == t2 -> Just t1 
                                                               | otherwise -> Nothing
                                            _ -> Nothing
                            _ -> Nothing
--procura o tipo da variável no contexto
typeof ctx (Var v) = lookup v ctx 
-- Caso de função (Lambda): a função recebe uma variável com tipo `t1` e retorna uma expressão `b`,
-- então o tipo da função é TFun (t1 -> t2), onde `t2` é o tipo da expressão `b` no contexto atualizado
typeof ctx (Lam v t1 b) = let Just t2 = typeof ((v, t1) : ctx) b 
                            in Just (TFun t1 t2)
-- Caso de aplicação de função: verifica se o primeiro operando é uma função e se o segundo operando
-- tem o tipo correto para ser aplicado. Retorna o tipo de retorno da função.
typeof ctx (App e1 e2) = case (typeof ctx e1, typeof ctx e2) of 
                           (Just (TFun t11 t12), Just t2) | t11 == t2 -> Just t12 
                                                          | otherwise -> Nothing 
                           _ -> Nothing
--verifica se o primeiro elemento tem o tipo correto para a lista
typeof ctx (Cons e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                            (Just t1, Just (TList t2)) | t1 == t2 -> Just (TList t1) --se os tipos combinam, retorna a lista
                            _ -> Nothing
--verifica se o tipo da expressão é uma lista e retorna TBool
typeof ctx (IsNil e) = case typeof ctx e of
                         Just (TList _) -> Just TBool
                         _ -> Nothing
--verifica se a expressão é uma lista e retorna o tipo do primeiro elemento
typeof ctx (Head e) = case typeof ctx e of
                        Just (TList t) -> Just t --retorna o tipo do primeiro elemento da lista
                        _ -> Nothing
--verifica se a expressão é uma lista e retorna uma lista com o mesmo tipo dos elementos
typeof ctx (Tail e) = case typeof ctx e of
                        Just (TList t) -> Just (TList t)
                        _ -> Nothing

typecheck :: Expr -> Expr 
typecheck e = case typeof [] e of 
                (Just _) -> e 
                _        -> error ("Erro verificando tipo da expressão: " ++ show e)
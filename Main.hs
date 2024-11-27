module Main where 

import Lexer 
import Parser
import Interpreter
import TypeChecker

main :: IO ()
main = do
    input <- readFile "exemplos/ex9.mylenguage"
    
    -- Tokeniza a entrada
    let tokens = lexer input
    
    -- Faz o parsing dos tokens
    let parsed = parser tokens
    
    print (eval parsed)
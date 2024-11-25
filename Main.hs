module Main where 

import Lexer 
import Parser
import Interpreter
import TypeChecker

main = do
    input <- readFile "./exemplos/ex3.mylenguage"  
    print . eval . typecheck . parser . lexer $ input

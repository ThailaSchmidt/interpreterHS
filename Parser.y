{
module Parser where 

import Lexer
}

%name parser 
%tokentype { Token }
%error { parseError } 

%token 
  true          { TokenTrue }
  false         { TokenFalse }
  num           { TokenNum $$ }
  '+'           { TokenAdd }
  '-'           { TokenSub }
  '*'           { TokenMul }
  and           { TokenAnd }
  or            { TokenOr }
  not           { TokenNot }
  "=="          { TokenEq }
  ">="          { TokenMrEq }
  if            { TokenIf }
  then          { TokenThen }
  else          { TokenElse }
  ":"            { TokenCons }
  "."            { TokenIsNil }
  "#"            { TokenHead }
  "@"            { TokenTail }

%nonassoc if then else 
%left "==" 
%left ">=" 
%left '+' '-' '*' 
%left "and"
%left "or"
%right "not"

%% 

-- Regras para expressões
Exp : true                        { BTrue }
    | false                       { BFalse }
    | num                         { Num $1 }
    | Exp '+' Exp                 { Add $1 $3 }
    | Exp '-' Exp                 { Sub $1 $3 }
    | Exp '*' Exp                 { Mul $1 $3 }
    | Exp and Exp                 { And $1 $3 }
    | Exp or Exp                  { Or $1 $3 }
    | Exp not Exp                 { Not $1 }
    | Exp "==" Exp                { Eq $1 $3 }
    | Exp ">=" Exp                { MrEq $1 $3 }
    | if Exp then Exp else Exp    { If $2 $4 $6 }
    -- Regras para expressões de listas e operações
    | Exp ":" Exp                 { Cons $1 $3 }       -- Construção de listas
    | "." Exp                     { IsNil $2 }         -- Verifica se uma lista está vazia
    | "#" Exp                     { Head $2 }          -- Cabeça de uma lista
    | "@" Exp                     { Tail $2 }          -- Cauda de uma lista   

{
parseError :: [Token] -> a 
parseError ts = error "Syntax error: sequência de instruções inválidas."
}
module Submission where

import System.IO
import Data.Char
import ParsePCFLet

data Value = NUM Int | BOOL Bool | SUCC | PRED | 
                       ISZERO | CLOSURE (String, Term, Env) | 
                       THUNK (Term, Env) | ERROR(String, Value) deriving (Eq, Show)

type Env = [(String, Value)]

{- Note that ERROR terms are used for debugging purposes when an
 error occurs during expression evaluation. -}

-- update the environment to associate identifier nustr with value nuval
update :: [(t, u)] -> t -> u -> [(t, u)] 
update environ nustr nuval = (nustr,nuval):environ;

-- Find the value of an identifier in an environment 
getVal :: [Char] -> [([Char], Value)] -> Value
getVal id [] = ERROR (id++" not in environment",NUM 0)
getVal id ((id2,val2):rest) = if (id == id2) then val2
                                     else getVal id rest;

-- Compute the value of a term given an environment providing values for free variables 
newinterp :: Term -> Env -> Value
newinterp (AST_NUM(n)) ev = NUM (n)

-- normally newinterp (AST_ID(id)) = getVal id ev
-- It's more complicated here because of recursion
newinterp (AST_ID(id)) ev = 
            let 
                process(THUNK(tm,oldev)) = newinterp tm oldev
                process(other) = other
            in 
                process(getVal id ev)
newinterp (AST_BOOL(bval)) ev = BOOL(bval)
newinterp (AST_FUN(param,body)) ev = CLOSURE(param,body,ev)
newinterp (AST_SUCC) ev = SUCC
newinterp (AST_PRED) ev = PRED
newinterp (AST_ISZERO) ev = ISZERO
newinterp (AST_ERROR str) ev = ERROR ("In parse", NUM 0)

-- conditional statement
newinterp (AST_IF(test,yesval,noval)) ev = 
        let -- watch out for case where test evaluates to non-boolean
           testval = newinterp test ev
           trueval(BOOL(True)) = True
           trueval(x) = False
           falseval(BOOL(False)) = True
           falseval(x) = False;
        in 
           if trueval(testval) then newinterp yesval ev
                 else if falseval(testval) then newinterp noval ev
                else ERROR ("if cond not bool",testval)

-- Function application
newinterp (AST_APP(func,arg)) ev = 
        let
            evalfunc = newinterp func ev -- evaluate func and arg in ev before pattern matching
            evalarg = newinterp arg ev
            eval(SUCC,NUM(n)) = NUM(n+1)
            eval(PRED,NUM(n)) = if n > 0 then NUM (n-1)
                                         else NUM 0
            eval(ISZERO,NUM(n)) = if n == 0 then BOOL(True)
                                           else BOOL(False)
            eval(CLOSURE(param,body,fev),ERROR (s,v)) = ERROR ((s++" in arg"),v)
            eval(CLOSURE(param,body,fev),arg) =  
                                        let
                                           nuev = update fev param arg
                                        in 
                                           newinterp body nuev
            eval(fst,snd) = ERROR ("no fcn",fst)
        in 
            eval(evalfunc,evalarg)

-- recursion - updates environment with THUNK for recursion.
newinterp (AST_REC(name,body)) ev = 
            let   -- create new environment so name associated with 
                  -- thunk with term and environment
               nuev = update ev name (THUNK(AST_REC(name,body),ev))
            in 
               newinterp body nuev


-- (e1, env) -> v1  (e2, env[v1/x]) -> v2 / (let x = e1 in e2 end, env) -> v2
newinterp (AST_LET(x, e1, e2)) ev =
          let   
              v1 = newinterp e1 ev -- evaluate e1 in env, save it to a new value
              ev2 = update ev x v1 -- update environment with x saved to new value
          in
              newinterp e2 ev2 -- return interpretation of e2 in new env
              

main3 :: IO()
main3 = do
          putStrLn "Type a file name holding a PCF expression"
          fileName <- getLine
          infileHandle <- openFile fileName ReadMode
          pcfExp <- hGetLine infileHandle
          putStr("The value of "++(show pcfExp)++" is ")
          putStrLn (show (newinterp(parsestr pcfExp)[]))
          hClose infileHandle

-- (haskell module: (module) (where)
module Main where

-- (import (qualified_module (module) (module)) (module))
import Data.Map as Map
-- (import (qualified_module (module) (module)) (import_list (import_item (variable)) (comma) (import_item (variable))))
import Data.Set hiding (map, filter)
-- (import (qualified_module (module) (module)) (import_list (import_item (variable))))
import Process.Exit (exitFailure)
-- (import (qualified_module (module) (module)))
import System.Environment

-- (signature name: (variable) type: (type_apply (type_name (type)) (type_literal (con_unit))))
main :: IO ()
-- (function name: (variable) rhs: (exp_apply (exp_name (variable)) (exp_literal (string))))
main = putStrLn "Hello, World!"

-- (signature name: (variable) type: (type_apply (type_name (type)) (type_literal (con_unit))))
hasLet :: IO ()
-- (function name: (variable) rhs: (_))
hasLet =
  -- (exp_let_in (exp_let (decls (function name: (variable) rhs: (exp_literal (integer))))) (exp_in (exp_apply (exp_name (variable)) (exp_name (variable)))))
  let x = 1
   in print x

-- (signature name: (variable) type: (type_apply (type_name (type)) (type_literal (con_unit))))
hasWhere :: IO ()
-- (function name: (variable) rhs: (exp_apply (exp_name (variable)) (exp_name (variable))) (where) (decls (function name: (variable) rhs: (exp_literal (integer)))))
hasWhere = print x
  where x = 1

-- (signature name: (variable) type: (type_apply (type_name (type)) (type_literal (con_unit))))
hasDoBinding :: IO ()
-- (function name: (variable) rhs: (exp_do (stmt (bind_pattern (pat_name (variable)) (exp_apply (exp_name (variable)) (exp_literal (string))))) (stmt (exp_apply (exp_name (variable)) (exp_name (variable))))))
hasDoBinding = do
  path <- getEnv "PATH"
  putStrLn path

-- (signature name: (variable) type: (type_apply (type_name (type)) (type_literal (con_unit))))
hasLetInDo :: IO ()
-- (function name: (variable) rhs: (exp_do (stmt (let (decls (function name: (variable) rhs: (exp_literal (integer)))))) (stmt (exp_apply (exp_name (variable)) (exp_name (variable))))))
hasLetInDo = do
  let x = 1
  print x

-- (signature name: (variable) type: (fun (type_name (type)) (fun (type_name (type)) (type_name (type)))))
functionWithArgs :: String -> String -> String
-- (function name: (variable) patterns: (patterns (pat_name (variable)) (pat_name (variable))) rhs: (exp_infix (exp_name (variable)) (operator) (exp_name (variable))))
functionWithArgs a b = a ++ b

-- (signature name: (variable) type: (fun (type_name (type)) (fun (type_name (type)) (type_name (type)))))
lambdaWithArgs :: String -> String -> String
-- (function name: (variable) rhs: (exp_lambda (pat_name (variable)) (pat_name (variable)) (exp_infix (exp_name (variable)) (operator) (exp_name (variable))))))
lambdaWithArgs = \a b -> a ++ b

data Union = Ctor1 Int | Ctor2 String

newtype Thing = Thing String

instance Show Thing where
  show (Thing inner) = inner

-- module Lusk where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Lusk.Parser
import Lusk.Eval

main :: IO ()
main = runErrorT (runStateT repl symbolTable) >> return ()
  where
    repl :: StateT SymbolTable (ErrorT String IO) ()
    repl = do
      liftIO (putStr "> ")
      input <- liftIO getLine 
      case readExpr input of
        Left err -> liftIO (putStrLn err)
        Right tree -> {-liftIO (putStrLn $ show tree)-} catchError (eval tree >>= \v -> liftIO (putStrLn $ show v))
                                                                (liftIO . putStrLn . ((++) "Error: "))
      repl
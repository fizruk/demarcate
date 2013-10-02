module Main where

import Common
import Control.Monad.State

type SP = P (StateT Int)

runSP :: SP a -> IO a
runSP = runP $ flip evalStateT 0

-- | Subprogram. Outputs current state.
outputState :: SP ()
outputState = do
    n <- get
    output $ "state: " ++ show n

-- | Program for our toy language.
prg :: SP ()
prg = do
    outputState

    output "Hello!"
    output "Enter your name, please:"
    s <- input
    output $ "Goodbye, " ++ s ++ "!"

    outputState

hack' :: SP a -> SP a
hack' = hack $ modify (+1)

-- | Running hacked program...
main :: IO ()
main = runSP $ hack' prg


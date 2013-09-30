{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Monad.Trans.Demarcate
import Control.Monad.Free
import Control.Monad.State

-- | Instructions for a toy language.
data ProgramF next
  = Output String next      -- output something
  | Input (String -> next)  -- get something

instance Functor ProgramF where
    fmap f (Output s x) = Output s (f x)
    fmap f (Input g)    = Input (f . g)

-- | Low-level program is just a Free monad over ProgramF.
type Program = Free ProgramF

-- | Output command.
output :: (MonadFree ProgramF m, MonadTrans t) => String -> t m ()
output s = lift . liftF $ Output s ()

-- | Input command.
input :: (MonadFree ProgramF m, MonadTrans t) => t m String
input = lift . liftF $ Input id

-- | SP stands for Stateful Program.
type SP = Demarcate (StateT Int) Program

-- | Interpreter for a low-level program.
runProgram :: Program a -> IO a
runProgram = iterM runProgramF
  where
    runProgramF (Output s next) = putStrLn s >> next
    runProgramF (Input next)    = getLine >>= next

-- | Interpreter for a stateful program
runSP :: SP a -> IO a
runSP = runProgram . flip evalStateT 0 . execDemarcate

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

-- | A hacking transformation.
-- It prepend to each low-level output some extra stateful code.
-- Other commands are left untouched.
hack :: SP a -> SP a
hack = transformDemarcateFree hackF
  where
    hackF :: ProgramF (SP a) -> SP a
    hackF cmd@(Output s next) = do
      modify (+1)
      output "*** incremented state"
      wrapT cmd
    hackF cmd =
      wrapT cmd

-- | Running hacked program...
main :: IO ()
main = runSP $ hack prg


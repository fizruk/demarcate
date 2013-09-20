{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import Control.Monad.Trans.Demarcate
import Control.Monad.Free
import Control.Monad.State

data ProgramF next
  = Output String next
  | Input (String -> next)

instance Functor ProgramF where
    fmap f (Output s x) = Output s (f x)
    fmap f (Input g)    = Input (f . g)

type Program = Free ProgramF

output :: (MonadFree ProgramF m, MonadTrans t) => String -> t m ()
output s = lift . liftF $ Output s ()

input :: (MonadFree ProgramF m, MonadTrans t) => t m String
input = lift . liftF $ Input id

type SP = Demarcate (StateT Int) Program

instance (MonadState s (t m)) => MonadState s (Demarcate t m) where
    get = demarcateT get
    put = demarcateT . put

runProgram :: Program a -> IO a
runProgram = iterM runProgramF
  where
    runProgramF (Output s next) = putStrLn s >> next
    runProgramF (Input next)    = getLine >>= next

runSP :: SP a -> IO a
runSP = runProgram . flip evalStateT 10 . execDemarcate

wrapCmd :: (Functor f, MonadTrans t, Monad (t (Free f))) => f (t (Free f) a) -> t (Free f) a
wrapCmd = join . lift . liftF

outputState :: SP ()
outputState = do
    n <- get
    output $ "state: " ++ show n

prg :: SP ()
prg = do
    outputState

    output "Hello!"
    output "Enter your name, please:"
    s <- input
    output $ "Goodbye, " ++ s ++ "!"

    outputState

hack :: SP a -> SP a
hack = transformDemarcateM $ iterM hackF
  where
    hackF :: ProgramF (SP a) -> SP a
    hackF cmd@(Output s next) = do
      modify (+1)
      output "*** incremented state"
      wrapCmd cmd
    hackF cmd =
      wrapCmd cmd

main :: IO ()
main = runSP $ hack prg


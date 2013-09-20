{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Monad.Trans.Demarcate
import Control.Monad.Free
import Control.Monad.State

-- | Substitute free monad actions with demarcated monad computations.
transformDemarcateFree :: (Functor f) =>
  (forall b. f (Demarcate t (Free f) b) -> Demarcate t (Free f) b) -> Demarcate t (Free f) a -> Demarcate t (Free f) a
transformDemarcateFree phi = transformDemarcateM (iterM phi)

-- | Helper function (useful with @transformDemarcateFree@).
-- I believe it should be somewhere in @Control.Monad.Free@
wrapInner :: (Functor f, MonadTrans t, Monad (t (Free f))) => f (t (Free f) a) -> t (Free f) a
wrapInner = join . lift . liftF

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

instance (MonadState s (t m)) => MonadState s (Demarcate t m) where
    get = demarcateT get
    put = demarcateT . put

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
      wrapInner cmd
    hackF cmd =
      wrapInner cmd

-- | Running hacked program...
main :: IO ()
main = runSP $ hack prg


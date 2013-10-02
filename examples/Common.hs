{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Common where

import Control.Monad
import Control.Monad.Free
import Control.Monad.Trans.Demarcate
import Control.Monad.Trans.Class

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

type P t = Demarcate t Program

-- | Interpreter for a low-level program.
runProgram :: Program a -> IO a
runProgram = iterM runProgramF
  where
    runProgramF (Output s next) = putStrLn s >> next
    runProgramF (Input next)    = getLine >>= next

runP :: (Monad (t Program), MonadTrans t) => (t Program a -> Program b) -> P t a -> IO b
runP runT = runProgram . runT . execDemarcate

-- | A hacking transformation.
-- It prepend to each low-level output some extra code.
-- Other commands are left untouched.
hack ::  P t ()-> P t a -> P t a
hack h = transformDemarcateFree hackF
  where
    hackF cmd@(Output s next) = do
      h
      output $ "[hacked] " ++ s
      next
    hackF cmd =
      wrapT cmd


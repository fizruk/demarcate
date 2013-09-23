{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import Control.Monad.Trans.Demarcate
import Control.Monad.Free
import Control.Monad.State
import Control.Monad.Reader

import Control.Concurrent.STM
import Control.Concurrent

-- | Instructions for a toy language.
data ProgramF next
  = Output String next      -- output something
  | Input (String -> next)  -- get something
  | GetNum (Int -> next)

instance Functor ProgramF where
    fmap f (Output s x) = Output s (f x)
    fmap f (Input g)    = Input (f . g)
    fmap f (GetNum g)   = GetNum (f . g)

-- | Low-level program is just a Free monad over ProgramF.
type Program = Free ProgramF

-- | Output command.
output :: (MonadFree ProgramF m, MonadTrans t) => String -> t m ()
output s = lift . liftF $ Output s ()

-- | Input command.
input :: (MonadFree ProgramF m, MonadTrans t) => t m String
input = lift . liftF $ Input id

-- | GetNum command.
getNum :: (MonadFree ProgramF m, MonadTrans t) => t m Int
getNum = lift . liftF $ GetNum id

-- | SP stands for Stateful Program.
type SP = Demarcate (StateT Int) Program

instance (MonadState s (t m)) => MonadState s (Demarcate t m) where
    get = demarcateT get
    put = demarcateT . put

type Env = ReaderT (TVar Int) IO

-- | Interpreter for a low-level program.
runProgram :: Program a -> Env a
runProgram = iterM runProgramF
  where
    runProgramF (Output s next) = lift (putStrLn s) >> next
    runProgramF (Input next)    = lift getLine >>= next
    runProgramF (GetNum next)   = ask >>= lift . atomically . readTVar >>= next

-- | Interpreter for a stateful program
runSP :: SP a -> Env a
runSP = runProgram . flip evalStateT 0 . execDemarcate

runEnv :: Int -> Env a -> IO a
runEnv n e = do
    var <- atomically $ newTVar n
    runReaderT e var

forkEnv :: Env a -> Env ()
forkEnv (ReaderT g) = ReaderT $ \e -> do
  forkIO (g e >> return ())
  return ()

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
    s <- getNum
    output $ "Goodbye, " ++ show s ++ "!"

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
main = runEnv 100500 $ do
  mapM_ (forkEnv . runSP . hack) [prg, prg, prg, prg, prg, prg, prg]


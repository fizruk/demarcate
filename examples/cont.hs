module Main where

import Common

import Control.Monad
import Control.Monad.Cont

type CP = P (ContT ())

runCP :: CP () -> IO ()
runCP = runP $ flip runContT return

-- | Program for our toy language.
prg :: CP ()
prg = do
    output "Hello!"
    output "Enter your name, please:"
    callCC $ \exit -> do
      name <- input
      callCC $ \_ -> do
        when (name == "") $ exit ()
        output $ "Goodbye, " ++ name ++ "!"
    output $ "The End"

hack' :: CP a -> CP a
hack' = hack $ do
  n <- callCC $ \exit -> do
    output "Extra check..."
    n <- input
    when (n == "") $ exit "Check failed!"
    return n
  output n

-- | Running hacked program...
main :: IO ()
main = runCP $ hack' prg


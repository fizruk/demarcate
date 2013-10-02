module Main where

import Common
import Control.Monad.Writer

type WP = P (WriterT String)

runWP :: WP a -> IO (a, String)
runWP = runP runWriterT

-- | Program for our toy language.
prg :: WP ()
prg = do
    output "Hello!"
    output "Enter your name, please:"
    (_, w) <- listen $ do
      name <- input
      tell $ "Got name: " ++ name ++ "\n"
      output $ "Goodbye, " ++ name ++ "!"
      return ((), (++ "ok"))
    output $ "Log was: " ++ w
    output $ "The End"

hack' :: WP a -> WP a
hack' = hack $ tell "boo!\n"

-- | Running hacked program...
main :: IO ()
main = do
  (_, log) <- runWP $ hack' prg
  putStrLn "==============================="
  putStrLn "Log:"
  putStrLn log


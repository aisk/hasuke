{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Agent (Agent (..), runTurn)
import Provider (AnthropicProvider (..), LLMProvider)
import Tool (execTool, fromList, readFileTool, writeFileTool)
import Types (Credentials (..), ProviderConfig (..), newSession)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline
import System.Environment (lookupEnv)
import System.Exit (die)
import System.IO (BufferMode (..), hSetBuffering, stdout)

defaultPrompt :: T.Text
defaultPrompt =
  "You are a helpful AI assistant with access to tools: \
  \read_file (read files), write_file (write files), exec (run shell commands). \
  \Be concise and accurate."

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  creds   <- loadCreds
  apiUrl <- maybe "https://api.anthropic.com" T.pack <$> lookupEnv "ANTHROPIC_BASE_URL"
  let cfg   = ProviderConfig creds apiUrl "claude-sonnet-4-6"
      prov  = AnthropicProvider cfg
      tools = fromList [readFileTool, writeFileTool, execTool]
  sess <- newSession
  let agent = Agent prov tools defaultPrompt 10 sess
  chatLoop agent

chatLoop :: LLMProvider p => Agent p -> IO ()
chatLoop agent = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing    -> return ()
        Just input
          | input `elem` ["exit", "quit"] -> return ()
          | null input                    -> loop
          | otherwise -> do
              result <- liftIO $ runTurn agent (T.pack input) TIO.putStr
              case result of
                Right _ -> liftIO $ TIO.putStrLn ""
                Left  err  -> liftIO $ TIO.putStrLn ("Error: " <> err)
              loop

loadCreds :: IO Credentials
loadCreds = do
  key   <- lookupEnv "ANTHROPIC_API_KEY"
  tok <- lookupEnv "ANTHROPIC_AUTH_TOKEN"
  case (key, tok) of
    (Just k, _) -> return (ApiKeyAuth (T.pack k))
    (_, Just t) -> return (BearerAuth (T.pack t))
    _           -> die "Set ANTHROPIC_API_KEY or ANTHROPIC_AUTH_TOKEN"

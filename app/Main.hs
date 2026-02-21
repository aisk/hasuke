{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Agent (Agent (..))
import Provider (AnthropicProvider (..))
import Tool (execTool, fromList, readFileTool, writeFileTool)
import Types (Credentials (..), ProviderConfig (..), ToolCall (..), newSession)
import UI (AppEvent (..), runUI)

import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.Exit (die)
import System.IO (BufferMode (..), hSetBuffering, stdout)

defaultPrompt :: T.Text
defaultPrompt =
  "You are a helpful AI assistant with access to tools: \
  \read_file (read files), write_file (write files), exec (run shell commands). \
  \Be concise and accurate."

confirmVia :: BChan AppEvent -> ToolCall -> IO Bool
confirmVia chan tc = do
  mvar <- newEmptyMVar
  writeBChan chan (ToolConfirmReq tc mvar)
  takeMVar mvar

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  creds  <- loadCreds
  apiUrl <- maybe "https://api.anthropic.com" T.pack <$> lookupEnv "ANTHROPIC_BASE_URL"
  let cfg   = ProviderConfig creds apiUrl "claude-sonnet-4-6"
      prov  = AnthropicProvider cfg
      tools = fromList [readFileTool, writeFileTool, execTool]
  sess <- newSession
  chan <- newBChan 100
  let agent = Agent prov tools (confirmVia chan) defaultPrompt 10 sess
  runUI agent chan

loadCreds :: IO Credentials
loadCreds = do
  key <- lookupEnv "ANTHROPIC_API_KEY"
  tok <- lookupEnv "ANTHROPIC_AUTH_TOKEN"
  case (key, tok) of
    (Just k, _) -> return (ApiKeyAuth (T.pack k))
    (_, Just t) -> return (BearerAuth (T.pack t))
    _           -> die "Set ANTHROPIC_API_KEY or ANTHROPIC_AUTH_TOKEN"

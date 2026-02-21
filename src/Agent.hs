{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent
  ( Agent (..)
  , runTurn
  ) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Provider (LLMProvider (..))
import Tool (ToolRegistry, runTool)
import Types

data Agent p = Agent
  { provider      :: p
  , registry      :: ToolRegistry
  , systemPrompt  :: Text
  , maxIterations :: Int
  , session       :: Session
  }

-- Process one user turn, update the session, and return the reply
runTurn :: LLMProvider p => Agent p -> Text -> (Text -> IO ()) -> IO (Either Text Text)
runTurn agent userInput onText = do
  history <- getHistory agent.session
  let initMsgs = Message System (String agent.systemPrompt)
               : history
              ++ [Message User (String userInput)]
  go initMsgs agent.maxIterations
  where
    go :: [Message] -> Int -> IO (Either Text Text)
    go _    0 = return (Left "Max iterations reached")
    go msgs n = do
      result <- chat agent.provider msgs (Map.elems agent.registry) onText
      case result of
        Left err -> return (Left err)
        Right resp
          | null resp.toolCalls -> do
              -- No tool calls: save to session and return the reply
              let reply = fromMaybe "" resp.content
              addMessages agent.session
                [ Message User      (String userInput)
                , Message Assistant (String reply)
                ]
              return (Right reply)
          | otherwise -> do
              -- Tool calls present: execute them, append results, and loop
              toolResults <- mapM (runTool agent.registry) resp.toolCalls
              let newMsgs = msgs
                    ++ [toAssistantMsg resp]
                    ++ map toToolResultMsg (zip resp.toolCalls toolResults)
              go newMsgs (n - 1)

-- Build an assistant message with tool_use content blocks (Anthropic API format)
toAssistantMsg :: LLMResponse -> Message
toAssistantMsg resp = Message Assistant (toJSON (textBlocks <> toolBlocks))
  where
    textBlocks = maybe [] (\t -> [object ["type" .= ("text" :: Text), "text" .= t]]) resp.content
    toolBlocks =
      [ object
          [ "type"  .= ("tool_use" :: Text)
          , "id"    .= tc.id
          , "name"  .= tc.name
          , "input" .= tc.arguments
          ]
      | tc <- resp.toolCalls
      ]

-- Build a user message with a tool_result content block (Anthropic API format)
toToolResultMsg :: (ToolCall, Either Text Text) -> Message
toToolResultMsg (tc, result) = Message User $ toJSON
  [ object
      [ "type"        .= ("tool_result" :: Text)
      , "tool_use_id" .= tc.id
      , "content"     .= case result of { Left t -> t; Right t -> t }
      ]
  ]

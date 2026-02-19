{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Types
  ( Role (..)
  , Message (..)
  , ToolCall (..)
  , LLMResponse (..)
  , Credentials (..)
  , ProviderConfig (..)
  , Session
  , newSession
  , addMessages
  , getHistory
  ) where

import Data.Aeson (Value)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Text (Text)

data Role = User | Assistant | System
  deriving (Eq, Show)

-- content is Value (not Text):
-- regular messages use String "text",
-- tool_use/tool_result messages use Array of content blocks
data Message = Message
  { role    :: Role
  , content :: Value
  } deriving (Show)

-- A single tool call returned by the LLM
-- arguments corresponds to the "input" field in the Anthropic API
data ToolCall = ToolCall
  { id        :: Text
  , name      :: Text
  , arguments :: Map Text Value
  } deriving (Show)

-- Parsed LLM response
data LLMResponse = LLMResponse
  { content   :: Maybe Text
  , toolCalls :: [ToolCall]
  } deriving (Show)

data Credentials
  = ApiKeyAuth { apiKey :: Text }
  | BearerAuth { token  :: Text }

data ProviderConfig = ProviderConfig
  { credentials :: Credentials
  , baseUrl     :: Text
  , model       :: Text
  }

newtype Session = Session (IORef [Message])

newSession :: IO Session
newSession = Session <$> newIORef []

addMessages :: Session -> [Message] -> IO ()
addMessages (Session ref) msgs = modifyIORef ref (<> msgs)

getHistory :: Session -> IO [Message]
getHistory (Session ref) = readIORef ref

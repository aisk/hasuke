{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Provider
  ( LLMProvider (..)
  , AnthropicProvider (..)
  ) where

import Data.Aeson (Object, Value (..), object, withObject, (.=), (.:))
import Data.Aeson.Types (Parser, parseEither)
import Data.List (partition)
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Request (Headers, Method (..), Request (..), Response (..), send)
import Tool (ToolDef (..))
import Types

class LLMProvider p where
  chat :: p -> [Message] -> [ToolDef] -> IO (Either Text LLMResponse)

newtype AnthropicProvider = AnthropicProvider { config :: ProviderConfig }

instance LLMProvider AnthropicProvider where
  chat p msgs tools = do
    let payload = buildPayload p.config msgs tools
        hdrs    = buildHeaders p.config.credentials
        apiUrl  = T.unpack p.config.baseUrl <> "/v1/messages"
        req     = Request POST apiUrl hdrs (Just payload)
    resp <- send req :: IO (Response Value)
    if resp.status >= 200 && resp.status < 300
      then return $ parseResponse resp.body
      else return $ Left ("API error " <> T.pack (show resp.status))

buildHeaders :: Credentials -> Headers
buildHeaders (ApiKeyAuth key) =
  [ ("x-api-key",         TE.encodeUtf8 key)
  , ("anthropic-version", "2023-06-01")
  ]
buildHeaders (BearerAuth tok) =
  [ ("Authorization", "Bearer " <> TE.encodeUtf8 tok)
  ]

buildPayload :: ProviderConfig -> [Message] -> [ToolDef] -> Value
buildPayload cfg msgs tools =
  object $ base <> sysField <> toolField
  where
    (sysMsgs, chatMsgs) = partition (\m -> m.role == System) msgs

    sysText = T.intercalate "\n" (catMaybes (map extractText sysMsgs))
    extractText :: Message -> Maybe Text
    extractText m = case m.content of
      String t -> Just t
      _        -> Nothing

    base =
      [ "model"      .= cfg.model
      , "max_tokens" .= (4096 :: Int)
      , "messages"   .= map toApiMsg chatMsgs
      ]

    sysField  = if T.null sysText then [] else ["system" .= sysText]
    toolField = if null tools     then [] else ["tools"  .= map toToolSchema tools]

    toApiMsg :: Message -> Value
    toApiMsg m = object
      [ "role"    .= roleText m.role
      , "content" .= m.content
      ]

    roleText :: Role -> Text
    roleText User      = "user"
    roleText Assistant = "assistant"
    roleText System    = "user"

    toToolSchema :: ToolDef -> Value
    toToolSchema t = object
      [ "name"         .= t.name
      , "description"  .= t.description
      , "input_schema" .= t.parameters
      ]

parseResponse :: Value -> Either Text LLMResponse
parseResponse v =
  case parseEither (withObject "response" parseResp) v of
    Left e  -> Left (T.pack e)
    Right r -> Right r

parseResp :: Object -> Parser LLMResponse
parseResp o = do
  blocks <- o .: "content" :: Parser [Value]
  texts  <- mapM parseTextBlock blocks
  calls  <- mapM parseToolBlock blocks
  return LLMResponse
    { content   = listToMaybe (catMaybes texts)
    , toolCalls = catMaybes calls
    }

parseTextBlock :: Value -> Parser (Maybe Text)
parseTextBlock = withObject "content block" $ \b -> do
  typ <- b .: "type" :: Parser Text
  if typ == "text"
    then Just <$> b .: "text"
    else return Nothing

parseToolBlock :: Value -> Parser (Maybe ToolCall)
parseToolBlock = withObject "content block" $ \b -> do
  typ <- b .: "type" :: Parser Text
  if typ == "tool_use"
    then do
      tcId   <- b .: "id"    :: Parser Text
      tcName <- b .: "name"  :: Parser Text
      tcArgs <- b .: "input" :: Parser (Map Text Value)
      return (Just (ToolCall tcId tcName tcArgs))
    else return Nothing

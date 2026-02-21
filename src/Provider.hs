{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Provider
  ( LLMProvider (..)
  , AnthropicProvider (..)
  ) where

import Data.Aeson (Value (..), object, withObject, (.=), (.:))
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Data.List (partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Request (Headers, Method (..), Request (..), Response (..), StreamBody (..), SseEvent (..), send)
import Tool (ToolDef (..))
import Types

class LLMProvider p where
  chat :: p -> [Message] -> [ToolDef] -> (Text -> IO ()) -> IO (Either Text LLMResponse)

newtype AnthropicProvider = AnthropicProvider { config :: ProviderConfig }

instance LLMProvider AnthropicProvider where
  chat p msgs tools onText = do
    let payload = buildPayload p.config msgs tools
        hdrs    = buildHeaders p.config.credentials
        apiUrl  = T.unpack p.config.baseUrl <> "/v1/messages"
        req     = Request POST apiUrl hdrs (Just payload)
    resp <- send req :: IO (Response (StreamBody SseEvent))
    if resp.status >= 200 && resp.status < 300
      then processStream onText resp
      else do
        resp.body.closeStream
        return $ Left ("API error " <> T.pack (show resp.status))

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
      , "stream"     .= True
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

data BlockAccum
  = TextAccum  !Text
  | ToolAccum  !Text !Text !Text  -- id, name, accumulated_partial_json

processStream :: (Text -> IO ()) -> Response (StreamBody SseEvent) -> IO (Either Text LLMResponse)
processStream onText resp = do
  stateRef <- newIORef (Map.empty :: Map Int BlockAccum)
  drainLoop stateRef resp.body.readNext
  resp.body.closeStream
  state <- readIORef stateRef
  return (Right (buildLLMResponse state))
  where
    drainLoop :: IORef (Map Int BlockAccum) -> IO (Maybe SseEvent) -> IO ()
    drainLoop ref fetchNext = do
      mEv <- fetchNext
      case mEv of
        Nothing -> return ()
        Just ev -> do
          handleSseEvent ref onText ev
          drainLoop ref fetchNext

handleSseEvent :: IORef (Map Int BlockAccum) -> (Text -> IO ()) -> SseEvent -> IO ()
handleSseEvent stateRef onText ev =
  case Aeson.decode (BSL.fromStrict (TE.encodeUtf8 ev.sseData)) of
    Nothing -> return ()
    Just v  ->
      case parseMaybe (withObject "ev" (.: "type")) v of
        Nothing  -> return ()
        Just typ -> case (typ :: Text) of
          "content_block_start" -> handleBlockStart stateRef v
          "content_block_delta" -> handleBlockDelta stateRef onText v
          _                     -> return ()

handleBlockStart :: IORef (Map Int BlockAccum) -> Value -> IO ()
handleBlockStart stateRef v =
  case parseMaybe parseStart v of
    Nothing         -> return ()
    Just (idx, blk) -> modifyIORef stateRef (Map.insert idx blk)
  where
    parseStart :: Value -> Parser (Int, BlockAccum)
    parseStart = withObject "start" $ \o -> do
      idx   <- o .: "index"
      block <- o .: "content_block" :: Parser Value
      parseBlock idx block

    parseBlock :: Int -> Value -> Parser (Int, BlockAccum)
    parseBlock idx = withObject "block" $ \b -> do
      btype <- b .: "type" :: Parser Text
      case btype of
        "text"     -> return (idx, TextAccum "")
        "tool_use" -> do
          tid   <- b .: "id"
          tname <- b .: "name"
          return (idx, ToolAccum tid tname "")
        _          -> fail "unknown block type"

handleBlockDelta :: IORef (Map Int BlockAccum) -> (Text -> IO ()) -> Value -> IO ()
handleBlockDelta stateRef onText v =
  case parseMaybe parseDelta v of
    Nothing              -> return ()
    Just (idx, Left  t)  -> do
      modifyIORef stateRef (Map.adjust (appendText t) idx)
      onText t
    Just (idx, Right j)  ->
      modifyIORef stateRef (Map.adjust (appendJson j) idx)
  where
    parseDelta :: Value -> Parser (Int, Either Text Text)
    parseDelta = withObject "delta_ev" $ \o -> do
      idx   <- o .: "index"
      delta <- o .: "delta" :: Parser Value
      parseDeltaContent idx delta

    parseDeltaContent :: Int -> Value -> Parser (Int, Either Text Text)
    parseDeltaContent idx = withObject "delta" $ \d -> do
      dtype <- d .: "type" :: Parser Text
      case dtype of
        "text_delta"       -> do t <- d .: "text";         return (idx, Left t)
        "input_json_delta" -> do j <- d .: "partial_json"; return (idx, Right j)
        _                  -> fail "unknown delta type"

appendText :: Text -> BlockAccum -> BlockAccum
appendText t (TextAccum s)     = TextAccum (s <> t)
appendText _ b                 = b

appendJson :: Text -> BlockAccum -> BlockAccum
appendJson j (ToolAccum i n s) = ToolAccum i n (s <> j)
appendJson _ b                 = b

buildLLMResponse :: Map Int BlockAccum -> LLMResponse
buildLLMResponse blocks = LLMResponse
  { content   = listToMaybe [t | TextAccum t <- sorted, not (T.null t)]
  , toolCalls = catMaybes [parseToolAccum i n j | ToolAccum i n j <- sorted]
  }
  where
    sorted = map snd (Map.toAscList blocks)

parseToolAccum :: Text -> Text -> Text -> Maybe ToolCall
parseToolAccum tcId tcName jsonStr =
  case Aeson.decode (BSL.fromStrict (TE.encodeUtf8 jsonStr)) of
    Just args -> Just (ToolCall tcId tcName args)
    Nothing   -> Nothing

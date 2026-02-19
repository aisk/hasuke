{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Tool
  ( Handler
  , ToolDef (..)
  , ToolRegistry
  , fromList
  , runTool
  , readFileTool
  , writeFileTool
  , execTool
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), object, (.=))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (readProcess)
import Types (ToolCall (..))

-- Type alias for a tool execution function
type Handler = Map Text Value -> IO (Either Text Text)

data ToolDef = ToolDef
  { name        :: Text
  , description :: Text
  , parameters  :: Value    -- JSON Schema, sent as "input_schema" to Anthropic API
  , handler     :: Handler
  }

type ToolRegistry = Map Text ToolDef

fromList :: [ToolDef] -> ToolRegistry
fromList tools = Map.fromList [(t.name, t) | t <- tools]

runTool :: ToolRegistry -> ToolCall -> IO (Either Text Text)
runTool reg tc =
  case Map.lookup tc.name reg of
    Nothing   -> return (Left ("Unknown tool: " <> tc.name))
    Just tool -> tool.handler tc.arguments

-- Helper to extract a string argument by key
getStringArg :: Map Text Value -> Text -> Either Text Text
getStringArg args key =
  case Map.lookup key args of
    Just (String s) -> Right s
    Just _          -> Left ("Expected string for argument '" <> key <> "'")
    Nothing         -> Left ("Missing required argument '" <> key <> "'")

readFileTool :: ToolDef
readFileTool = ToolDef
  { name        = "read_file"
  , description = "Read the contents of a file from the filesystem"
  , parameters  = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "path" .= object
              [ "type"        .= ("string" :: Text)
              , "description" .= ("Path to the file to read" :: Text)
              ]
          ]
      , "required" .= (["path"] :: [Text])
      ]
  , handler = \args ->
      case getStringArg args "path" of
        Left err -> return (Left err)
        Right p  -> do
          result <- try (TIO.readFile (T.unpack p)) :: IO (Either SomeException Text)
          return $ case result of
            Right contents -> Right contents
            Left  err      -> Left (T.pack (show err))
  }

writeFileTool :: ToolDef
writeFileTool = ToolDef
  { name        = "write_file"
  , description = "Write content to a file on the filesystem"
  , parameters  = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "path" .= object
              [ "type"        .= ("string" :: Text)
              , "description" .= ("Path to write to" :: Text)
              ]
          , "content" .= object
              [ "type"        .= ("string" :: Text)
              , "description" .= ("Content to write" :: Text)
              ]
          ]
      , "required" .= (["path", "content"] :: [Text])
      ]
  , handler = \args ->
      case (getStringArg args "path", getStringArg args "content") of
        (Right p, Right c) -> do
          result <- try (TIO.writeFile (T.unpack p) c) :: IO (Either SomeException ())
          return $ case result of
            Right () -> Right "Written successfully"
            Left err -> Left (T.pack (show err))
        (Left err, _) -> return (Left err)
        (_, Left err) -> return (Left err)
  }

execTool :: ToolDef
execTool = ToolDef
  { name        = "exec"
  , description = "Execute a shell command and return its output"
  , parameters  = object
      [ "type" .= ("object" :: Text)
      , "properties" .= object
          [ "command" .= object
              [ "type"        .= ("string" :: Text)
              , "description" .= ("Shell command to execute" :: Text)
              ]
          ]
      , "required" .= (["command"] :: [Text])
      ]
  , handler = \args ->
      case getStringArg args "command" of
        Left err  -> return (Left err)
        Right cmd -> do
          result <- try (readProcess "sh" ["-c", T.unpack cmd] "")
                    :: IO (Either SomeException String)
          return $ case result of
            Right output -> Right (T.pack output)
            Left  err    -> Left (T.pack (show err))
  }

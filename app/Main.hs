{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Agent (Agent (..), runTurn)
import Control.Exception (SomeException, finally, toException, try)
import Control.Monad (void)
import Data.IORef (newIORef, readIORef, writeIORef)
import Provider (AnthropicProvider (..), LLMProvider)
import Tool (execTool, fromList, readFileTool, writeFileTool)
import Types (Credentials (..), ProviderConfig (..), ToolCall (..), newSession)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline
import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import System.Exit (die)
import System.IO (BufferMode (..), Handle, hClose, hFlush, hSetBuffering, stdout)
import System.Process (ProcessHandle, StdStream (CreatePipe, Inherit), createProcess, proc, std_err, std_in, std_out, waitForProcess)

data RenderMode = Plain | Bat

data Renderer = Renderer
  { onChunk :: T.Text -> IO ()
  , onDone  :: IO ()
  , cleanup :: IO ()
  }

basePrompt :: T.Text
basePrompt =
  "You are a helpful AI assistant with access to tools: \
  \read_file (read files), write_file (write files), exec (run shell commands). \
  \Be concise and accurate."

buildSystemPrompt :: RenderMode -> T.Text
buildSystemPrompt mode =
  case mode of
    Plain -> basePrompt <> " Don't use Markdown when communicated with user."
    Bat   -> basePrompt

detectRenderMode :: IO RenderMode
detectRenderMode = do
  batPath <- findExecutable "bat"
  case batPath of
    Just _  -> return Bat
    Nothing -> return Plain

plainRenderer :: Renderer
plainRenderer = Renderer
  { onChunk = TIO.putStr
  , onDone = TIO.putStrLn ""
  , cleanup = return ()
  }

spawnBat :: IO (Either SomeException (Handle, ProcessHandle))
spawnBat = do
  created <- try (createProcess (proc "bat" ["--paging=never", "-l", "markdown", "-p"])
    { std_in = CreatePipe
    , std_out = Inherit
    , std_err = Inherit
    }) :: IO (Either SomeException (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
  case created of
    Left err -> return (Left err)
    Right (Nothing, _, _, _) -> return (Left (toException (userError "bat stdin unavailable")))
    Right (Just hin, _, _, ph) -> return (Right (hin, ph))

closeRendererProcess :: Handle -> ProcessHandle -> IO ()
closeRendererProcess hin ph = do
  _ <- try (hClose hin) :: IO (Either SomeException ())
  _ <- try (void (waitForProcess ph)) :: IO (Either SomeException ())
  return ()

mkRenderer :: RenderMode -> IO Renderer
mkRenderer Plain = return plainRenderer
mkRenderer Bat = do
  started <- spawnBat
  case started of
    Left _ -> return plainRenderer
    Right (hin, ph) -> do
      fallbackRef <- newIORef False
      let renderChunk t = do
            fallback <- readIORef fallbackRef
            if fallback
              then TIO.putStr t
              else do
                r <- try (TIO.hPutStr hin t >> hFlush hin) :: IO (Either SomeException ())
                case r of
                  Right () -> return ()
                  Left _   -> do
                    writeIORef fallbackRef True
                    TIO.putStr t
          finishTurn = do
            fallback <- readIORef fallbackRef
            if fallback
              then TIO.putStrLn ""
              else do
                r <- try (TIO.hPutStr hin "\n" >> hFlush hin) :: IO (Either SomeException ())
                case r of
                  Right () -> return ()
                  Left _   -> do
                    writeIORef fallbackRef True
                    TIO.putStrLn ""
          cleanupRenderer = closeRendererProcess hin ph
      return Renderer
        { onChunk = renderChunk
        , onDone = finishTurn
        , cleanup = cleanupRenderer
        }

withRenderer :: RenderMode -> (Renderer -> IO a) -> IO a
withRenderer mode action = do
  renderer <- mkRenderer mode
  action renderer `finally` cleanup renderer

confirmTool :: ToolCall -> IO Bool
confirmTool tc = do
  TIO.putStrLn ""
  TIO.putStr ("Tool call: " <> tc.name <> " ")
  BSLC.putStrLn (Aeson.encode tc.arguments)
  TIO.putStr "Execute? [Y/n] "
  hFlush stdout
  answer <- getLine
  return (answer `notElem` ["n", "N"])

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  renderMode <- detectRenderMode
  creds   <- loadCreds
  apiUrl <- maybe "https://api.anthropic.com" T.pack <$> lookupEnv "ANTHROPIC_BASE_URL"
  let cfg   = ProviderConfig creds apiUrl "claude-sonnet-4-6"
      prov  = AnthropicProvider cfg
      tools = fromList [readFileTool, writeFileTool, execTool]
  sess <- newSession
  let sysPrompt = buildSystemPrompt renderMode
      agent = Agent prov tools confirmTool sysPrompt 10 sess
  chatLoop agent renderMode

chatLoop :: LLMProvider p => Agent p -> RenderMode -> IO ()
chatLoop agent renderMode = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing    -> return ()
        Just input
          | input `elem` ["exit", "quit"] -> return ()
          | null input                    -> loop
          | otherwise -> do
              result <- liftIO $ withRenderer renderMode $ \renderer -> do
                turnResult <- runTurn agent (T.pack input) (onChunk renderer)
                onDone renderer
                return turnResult
              case result of
                Right _ -> return ()
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

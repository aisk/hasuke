{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module UI
  ( AppEvent (..)
  , runUI
  ) where

import Agent (Agent (..), runTurn)
import Markdown (mdAttrs, renderMarkdown)
import Provider (LLMProvider)
import Types (Role (..), ToolCall (..))

import qualified Brick
import qualified Brick.AttrMap as BA
import Brick.BChan (BChan, writeBChan)
import qualified Brick.Main as BM
import qualified Brick.Types as BT
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Core as BWC
import qualified Brick.Widgets.Edit as BWE
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, putMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Config (defaultConfig, configInputMap)

data Name = ChatViewport | InputEditor
  deriving (Eq, Ord, Show)

data AppEvent
  = StreamChunk Text
  | StreamDone
  | ToolConfirmReq ToolCall (MVar Bool)
  | StreamError Text

data AppMode
  = NormalMode
  | WaitingMode
  | ConfirmMode ToolCall (MVar Bool)

data ChatEntry = ChatEntry Role Text

data AppState p = AppState
  { appMode     :: AppMode
  , history     :: [ChatEntry]
  , streaming   :: Text
  , inputEditor :: BWE.Editor Text Name
  , agentRef    :: Agent p
  , evChan      :: BChan AppEvent
  }

-- ---------------------------------------------------------------------------
-- Attributes
-- ---------------------------------------------------------------------------

theMap :: BA.AttrMap
theMap = BA.attrMap V.defAttr $
  [ (BWE.editAttr,        V.defAttr)
  , (BWE.editFocusedAttr, V.defAttr)
  ] ++ mdAttrs

-- ---------------------------------------------------------------------------
-- Drawing
-- ---------------------------------------------------------------------------

renderEntry :: ChatEntry -> BT.Widget Name
renderEntry (ChatEntry User t)      = BWC.vBox
  [ B.border $ BWC.padRight BWC.Max $ BWC.txtWrap t
  , BWC.txt "\8203"
  ]
renderEntry (ChatEntry Assistant t) = BWC.vBox
  [ BWC.padLeftRight 1 $ BWC.vBox $ renderMarkdown t
  , BWC.txt "\8203"
  ]
renderEntry (ChatEntry System _)    = BWC.emptyWidget

drawUI :: AppState p -> [BT.Widget Name]
drawUI st = [BWC.vBox [chatArea, BWC.txt "\8203", inputArea, statusBar]]
  where
    chatArea :: BT.Widget Name
    chatArea = BWC.viewport ChatViewport BT.Vertical $
      BWC.vBox (map renderEntry st.history ++ streamingWidget)

    streamingWidget :: [BT.Widget Name]
    streamingWidget
      | T.null st.streaming = []
      | otherwise           =
          [ BWC.padLeftRight 1 $ BWC.vBox $ renderMarkdown st.streaming
          ]

    statusBar :: BT.Widget Name
    statusBar = BWC.vLimit 1 $ case st.appMode of
      WaitingMode     -> BWC.txt " Waiting for response..."
      ConfirmMode tc _ ->
        BWC.txt (" Tool: " <> tc.name <> " " <> T.pack (BSLC.unpack (Aeson.encode tc.arguments)) <> "  [y] Execute  [n] Skip")
      _               -> BWC.txt " "

    inputArea :: BT.Widget Name
    inputArea = B.border editorWidget

    editorWidget :: BT.Widget Name
    editorWidget =
      let h = min 5 $ max 1 $ length (BWE.getEditContents st.inputEditor)
      in BWC.vLimit h $ BWE.renderEditor (BWC.txt . T.unlines) True st.inputEditor

-- ---------------------------------------------------------------------------
-- App event handler (AppEvent only, mode-independent)
-- ---------------------------------------------------------------------------

handleAppEv :: AppEvent -> BT.EventM Name (AppState p) ()
handleAppEv (StreamChunk t) = do
  Brick.modify $ \s -> s { streaming = s.streaming <> t }
  BM.vScrollToEnd (BM.viewportScroll ChatViewport)
handleAppEv StreamDone =
  Brick.modify $ \s ->
    let finalHistory
          | T.null s.streaming = s.history
          | otherwise          = s.history <> [ChatEntry Assistant s.streaming]
    in s { appMode = NormalMode, history = finalHistory, streaming = "" }
handleAppEv (ToolConfirmReq tc mvar) =
  Brick.modify $ \s ->
    let newHistory
          | T.null s.streaming = s.history
          | otherwise          = s.history <> [ChatEntry Assistant s.streaming]
    in s { appMode = ConfirmMode tc mvar, history = newHistory, streaming = "" }
handleAppEv (StreamError e) =
  Brick.modify $ \s ->
    s { appMode = NormalMode
      , history  = s.history <> [ChatEntry Assistant ("Error: " <> e)]
      , streaming = ""
      }

-- ---------------------------------------------------------------------------
-- Main event handler
-- ---------------------------------------------------------------------------

handleEvent :: LLMProvider p => BT.BrickEvent Name AppEvent -> BT.EventM Name (AppState p) ()

-- App channel events are handled regardless of mode
handleEvent (BT.AppEvent ae) = handleAppEv ae

-- Ctrl+C always quits
handleEvent (BT.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = Brick.halt

-- Shift+Enter inserts a newline in NormalMode
handleEvent (BT.VtyEvent (V.EvKey V.KEnter [V.MShift])) = do
  st <- Brick.get
  case st.appMode of
    NormalMode -> do
      (newEd, ()) <- BT.nestEventM st.inputEditor
        (BWE.handleEditorEvent (BT.VtyEvent (V.EvKey V.KEnter [])))
      Brick.put st { inputEditor = newEd }
    _ -> return ()

-- Enter submits input in NormalMode
handleEvent (BT.VtyEvent (V.EvKey V.KEnter [])) = do
  st <- Brick.get
  case st.appMode of
    NormalMode -> do
      let inputText = T.strip $ T.unlines $ BWE.getEditContents st.inputEditor
      if T.null inputText
        then return ()
        else do
          let chan = st.evChan
              agnt = st.agentRef
          Brick.put st
            { appMode     = WaitingMode
            , history     = st.history <> [ChatEntry User inputText]
            , inputEditor = BWE.editor InputEditor Nothing ""
            }
          _ <- liftIO $ forkIO $ do
            result <- runTurn agnt inputText (\t -> writeBChan chan (StreamChunk t))
            case result of
              Right _  -> writeBChan chan StreamDone
              Left err -> writeBChan chan (StreamError err)
          return ()
    _ -> return ()

-- All other events: mode-specific handling
handleEvent ev = do
  st <- Brick.get
  case st.appMode of
    NormalMode -> do
      (newEd, ()) <- BT.nestEventM st.inputEditor (BWE.handleEditorEvent ev)
      Brick.put st { inputEditor = newEd }
    WaitingMode -> do
      (newEd, ()) <- BT.nestEventM st.inputEditor (BWE.handleEditorEvent ev)
      Brick.put st { inputEditor = newEd }
    ConfirmMode _ mvar ->
      case ev of
        BT.VtyEvent (V.EvKey (V.KChar 'y') []) -> do
          liftIO $ putMVar mvar True
          Brick.modify $ \s -> s { appMode = WaitingMode }
        BT.VtyEvent (V.EvKey (V.KChar 'n') []) -> do
          liftIO $ putMVar mvar False
          Brick.modify $ \s -> s { appMode = WaitingMode }
        _ -> return ()
    _ -> return ()

-- ---------------------------------------------------------------------------
-- App definition and entry point
-- ---------------------------------------------------------------------------

theApp :: LLMProvider p => BM.App (AppState p) AppEvent Name
theApp = BM.App
  { BM.appDraw         = drawUI
  , BM.appChooseCursor = BM.showFirstCursor
  , BM.appHandleEvent  = handleEvent
  , BM.appStartEvent   = return ()
  , BM.appAttrMap      = const theMap
  }

runUI :: LLMProvider p => Agent p -> BChan AppEvent -> IO ()
runUI agent chan = do
  let initialState = AppState
        { appMode     = NormalMode
        , history     = []
        , streaming   = ""
        , inputEditor = BWE.editor InputEditor Nothing ""
        , agentRef    = agent
        , evChan      = chan
        }
  let cfg = defaultConfig { configInputMap = [(Nothing, "\ESC[27;2;13~", V.EvKey V.KEnter [V.MShift])] }
  vty <- mkVty cfg
  _ <- BM.customMain vty (mkVty cfg) (Just chan) theApp initialState
  return ()

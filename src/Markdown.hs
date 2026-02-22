{-# LANGUAGE OverloadedStrings #-}

module Markdown
  ( renderMarkdown
  , mdAttrs
  ) where

import qualified Brick.AttrMap as BA
import qualified Brick.Types as BT
import qualified Brick.Widgets.Core as BWC
import qualified Brick.Widgets.Table as BWT
import qualified CMarkGFM as CM
import           Data.Text (Text)
import qualified Data.Text as T
import           Lens.Micro ((^.))
import qualified Graphics.Vty as V

-- ---------------------------------------------------------------------------
-- Attribute names
-- ---------------------------------------------------------------------------

h1Attr, h2Attr, h3Attr :: BA.AttrName
h1Attr = BA.attrName "md.h1"
h2Attr = BA.attrName "md.h2"
h3Attr = BA.attrName "md.h3"

strongAttr, emphAttr, codeInlineAttr, linkAttr, plainAttr :: BA.AttrName
strongAttr     = BA.attrName "md.strong"
emphAttr       = BA.attrName "md.emph"
codeInlineAttr = BA.attrName "md.code.inline"
linkAttr       = BA.attrName "md.link"
plainAttr      = BA.attrName "md.plain"

codeBlockAttr, blockquoteAttr, tableHeaderAttr :: BA.AttrName
codeBlockAttr   = BA.attrName "md.code.block"
blockquoteAttr  = BA.attrName "md.blockquote"
tableHeaderAttr = BA.attrName "md.table.header"

-- ---------------------------------------------------------------------------
-- Color scheme
-- ---------------------------------------------------------------------------

mdAttrs :: [(BA.AttrName, V.Attr)]
mdAttrs =
  [ (h1Attr,          V.defAttr `V.withForeColor` V.cyan   `V.withStyle` V.bold)
  , (h2Attr,          V.defAttr `V.withForeColor` V.cyan)
  , (h3Attr,          V.defAttr `V.withForeColor` V.yellow `V.withStyle` V.bold)
  , (strongAttr,      V.defAttr `V.withStyle` V.bold)
  , (emphAttr,        V.defAttr `V.withForeColor` V.yellow)
  , (codeInlineAttr,  V.defAttr `V.withForeColor` V.green)
  , (linkAttr,        V.defAttr `V.withForeColor` V.blue)
  , (plainAttr,       V.defAttr)
  , (codeBlockAttr,   V.defAttr `V.withForeColor` V.green)
  , (blockquoteAttr,  V.defAttr `V.withForeColor` V.magenta)
  , (tableHeaderAttr, V.defAttr `V.withStyle` V.bold)
  ]

-- ---------------------------------------------------------------------------
-- Block rendering
-- ---------------------------------------------------------------------------

renderMarkdown :: Text -> [BT.Widget n]
renderMarkdown t =
  case CM.commonmarkToNode [] [CM.extTable, CM.extStrikethrough] t of
    CM.Node _ CM.DOCUMENT children -> concatMap blockToWidgets children
    _                               -> [BWC.txtWrap t]

blockToWidgets :: CM.Node -> [BT.Widget n]
blockToWidgets (CM.Node _ CM.PARAGRAPH children) =
  [BWC.padRight BWC.Max $ styledWrap (inlinesToSegs plainAttr children)]
blockToWidgets (CM.Node _ (CM.HEADING lvl) children) =
  let attr = headerAttr lvl
  in [BWC.withAttr attr $ BWC.padRight BWC.Max $
      BWC.txtWrap (extractText children)]
blockToWidgets (CM.Node _ (CM.CODE_BLOCK _ code) _) =
  [BWC.withAttr codeBlockAttr $ BWC.padRight BWC.Max $ BWC.txtWrap code]
blockToWidgets (CM.Node _ CM.BLOCK_QUOTE children) =
  [BWC.withAttr blockquoteAttr $ BWC.padLeft (BWC.Pad 2) $
   BWC.vBox (concatMap blockToWidgets children)]
blockToWidgets (CM.Node _ (CM.LIST attrs) children) =
  [BWC.vBox $ zipWith (renderItem attrs) [1 :: Int ..] children]
blockToWidgets (CM.Node _ CM.THEMATIC_BREAK _) =
  [BWC.padRight BWC.Max $ BWC.txt (T.replicate 40 "\x2500")]
blockToWidgets (CM.Node _ (CM.TABLE _aligns) children) =
  [renderTable children]
blockToWidgets _ = []

headerAttr :: Int -> BA.AttrName
headerAttr 1 = h1Attr
headerAttr 2 = h2Attr
headerAttr _ = h3Attr

renderItem :: CM.ListAttributes -> Int -> CM.Node -> BT.Widget n
renderItem attrs n (CM.Node _ CM.ITEM children) =
  let bullet = case CM.listType attrs of
        CM.BULLET_LIST  -> "\x2022 "
        CM.ORDERED_LIST -> T.pack (show n) <> ". "
  in BWC.txt bullet BWC.<+>
     BWC.vBox (concatMap blockToWidgets children)
renderItem _ _ node = BWC.vBox (blockToWidgets node)

-- ---------------------------------------------------------------------------
-- Inline rendering: styled segments + word-wrap widget
-- ---------------------------------------------------------------------------

type StyledSeg = (Text, BA.AttrName)

inlinesToSegs :: BA.AttrName -> [CM.Node] -> [StyledSeg]
inlinesToSegs cur = concatMap (inlineToSeg cur)

inlineToSeg :: BA.AttrName -> CM.Node -> [StyledSeg]
inlineToSeg cur (CM.Node _ (CM.TEXT t) _)    = [(t, cur)]
inlineToSeg cur (CM.Node _ CM.SOFTBREAK _)   = [(" ", cur)]
inlineToSeg cur (CM.Node _ CM.LINEBREAK _)   = [("\n", cur)]
inlineToSeg _   (CM.Node _ CM.STRONG cs)     = inlinesToSegs strongAttr cs
inlineToSeg _   (CM.Node _ CM.EMPH cs)       = inlinesToSegs emphAttr cs
inlineToSeg _   (CM.Node _ (CM.CODE t) _)    = [(t, codeInlineAttr)]
inlineToSeg _   (CM.Node _ (CM.LINK _ _) cs) = inlinesToSegs linkAttr cs
inlineToSeg cur (CM.Node _ _ cs)             = inlinesToSegs cur cs

-- Word-wrapping widget for mixed inline styles
styledWrap :: [StyledSeg] -> BT.Widget n
styledWrap segs = BT.Widget BT.Greedy BT.Fixed $ do
  ctx <- BT.getContext
  let avail = ctx ^. BT.availWidthL
      toks  = concatMap tokenizeSeg segs
      lns   = greedyLines avail toks
  BT.render $ BWC.vBox
    [ BWC.padRight BWC.Max $
      BWC.hBox [ BWC.withAttr a (BWC.txt w) | (w, a) <- ln ]
    | ln <- lns ]

-- Split a segment into word and space tokens
tokenizeSeg :: StyledSeg -> [StyledSeg]
tokenizeSeg (t, attr) =
  map (\w -> (w, attr)) $ filter (not . T.null) $
  T.groupBy (\a b -> (a == ' ') == (b == ' ')) t

-- Greedy line wrapping
greedyLines :: Int -> [StyledSeg] -> [[StyledSeg]]
greedyLines avail toks =
  let (lastLine, acc) = foldl' step ([], []) toks
  in reverse $ if null lastLine then acc else reverse lastLine : acc
  where
    step (cur, acc) (w, a)
      | w == "\n" =
          ([], reverse cur : acc)
      | T.all (== ' ') w =
          if null cur then (cur, acc)
          else let c = lineW cur
               in if c + displayWidth w > avail
                  then ([], reverse cur : acc)
                  else ((w, a) : cur, acc)
      | otherwise =
          let c = lineW cur
          in if c + displayWidth w > avail && c > 0
             then ([(w, a)], reverse cur : acc)
             else ((w, a) : cur, acc)
    lineW = sum . map (displayWidth . fst)

-- ---------------------------------------------------------------------------
-- Table rendering via Brick.Widgets.Table
-- ---------------------------------------------------------------------------

renderTable :: [CM.Node] -> BT.Widget n
renderTable rowNodes =
  case map extractWidgetRow rowNodes of
    [] -> BWC.emptyWidget
    (hdrCells : bodyCells) ->
      BWT.renderTable $
      BWT.surroundingBorder False $
      BWT.columnBorders True $
      BWT.rowBorders True $
      BWT.table $
        map (BWC.withAttr tableHeaderAttr) hdrCells : bodyCells

extractWidgetRow :: CM.Node -> [BT.Widget n]
extractWidgetRow (CM.Node _ CM.TABLE_ROW cells) = map extractWidgetCell cells
extractWidgetRow _                               = []

extractWidgetCell :: CM.Node -> BT.Widget n
extractWidgetCell (CM.Node _ CM.TABLE_CELL children) =
  BWC.padLeft (BWC.Pad 1) $ BWC.padRight (BWC.Pad 1) $
  BWC.txt (extractText children)
extractWidgetCell _ = BWC.emptyWidget

-- ---------------------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------------------

-- CJK-aware display width
displayWidth :: Text -> Int
displayWidth = T.foldl' (\acc c -> acc + charDispWidth c) 0

charDispWidth :: Char -> Int
charDispWidth c
  | isWideChar c = 2
  | otherwise    = 1

isWideChar :: Char -> Bool
isWideChar c =
  let o = fromEnum c
  in (o >= 0x1100 && o <= 0x115F) ||
     o == 0x2329 || o == 0x232A   ||
     (o >= 0x2E80 && o <= 0x303E) ||
     (o >= 0x3040 && o <= 0x33FF) ||
     (o >= 0x3400 && o <= 0x4DBF) ||
     (o >= 0x4E00 && o <= 0xA4CF) ||
     (o >= 0xA960 && o <= 0xA97F) ||
     (o >= 0xAC00 && o <= 0xD7FF) ||
     (o >= 0xF900 && o <= 0xFAFF) ||
     (o >= 0xFE10 && o <= 0xFE19) ||
     (o >= 0xFE30 && o <= 0xFE6F) ||
     (o >= 0xFF01 && o <= 0xFF60) ||
     (o >= 0xFFE0 && o <= 0xFFE6)

-- Extract plain text from inline nodes (for headings and table cells)
extractText :: [CM.Node] -> Text
extractText = T.concat . map nodeText
  where
    nodeText :: CM.Node -> Text
    nodeText (CM.Node _ (CM.TEXT t) _)  = t
    nodeText (CM.Node _ CM.SOFTBREAK _) = " "
    nodeText (CM.Node _ CM.LINEBREAK _) = "\n"
    nodeText (CM.Node _ (CM.CODE t) _)  = t
    nodeText (CM.Node _ _ cs)           = extractText cs

{-# LANGUAGE OverloadedStrings, TupleSections, ScopedTypeVariables #-}
{-
Copyright (C) 2006-2013 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Writers.Markdown
   Copyright   : Copyright (C) 2006-2013 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to markdown-formatted plain text.

Markdown:  <http://daringfireball.net/projects/markdown/>
-}
module Text.Pandoc.Writers.Markdown (writeMarkdown, writePlain) where
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Shared
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (blankline, char, space)
import Data.List ( group, isPrefixOf, find, intersperse, transpose, sortBy )
import Data.Char ( isSpace, isPunctuation )
import Data.Ord ( comparing )
import Text.Pandoc.Pretty
import Control.Monad.State
import qualified Data.Set as Set
import Text.Pandoc.Writers.HTML (writeHtmlString)
import Text.Pandoc.Readers.TeXMath (readTeXMath')
import Text.HTML.TagSoup (renderTags, parseTags, isTagText, Tag(..))
import Network.URI (isURI)
import Data.Default
import Data.Yaml (Value(Object,String,Array,Bool,Number))
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.Text as T

type Notes = [[Block]]
type Refs = [([Inline], Target)]
data WriterState = WriterState { stNotes :: Notes
                               , stRefs  :: Refs
                               , stIds   :: [String]
                               , stPlain :: Bool }
instance Default WriterState
  where def = WriterState{ stNotes = [], stRefs = [], stIds = [], stPlain = False }

-- | Convert Pandoc to Markdown.
writeMarkdown :: WriterOptions -> Pandoc -> String
writeMarkdown opts document =
  evalState (pandocToMarkdown opts{
                  writerWrapText = writerWrapText opts &&
                  not (isEnabled Ext_hard_line_breaks opts) }
             document) def

-- | Convert Pandoc to plain text (like markdown, but without links,
-- pictures, or inline formatting).
writePlain :: WriterOptions -> Pandoc -> String
writePlain opts document =
  evalState (pandocToMarkdown opts{
                 writerExtensions = Set.delete Ext_escaped_line_breaks $
                                    writerExtensions opts }
              document') def{ stPlain = True }
    where document' = plainify document

plainify :: Pandoc -> Pandoc
plainify = walk go
  where go :: Inline -> Inline
        go (Emph xs) = SmallCaps xs
        go (Strong xs) = SmallCaps xs
        go (Strikeout xs) = SmallCaps xs
        go (Superscript xs) = SmallCaps xs
        go (Subscript xs) = SmallCaps xs
        go (SmallCaps xs) = SmallCaps xs
        go (Code _ s) = Str s
        go (Math _ s) = Str s
        go (RawInline _ _) = Str ""
        go (Link xs _) = SmallCaps xs
        go (Image xs _) = SmallCaps $ [Str "["] ++ xs ++ [Str "]"]
        go (Cite _ cits) = SmallCaps cits
        go x = x

pandocTitleBlock :: Doc -> [Doc] -> Doc -> Doc
pandocTitleBlock tit auths dat =
  hang 2 (text "% ") tit <> cr <>
  hang 2 (text "% ") (vcat $ map nowrap auths) <> cr <>
  hang 2 (text "% ") dat <> cr

mmdTitleBlock :: Value -> Doc
mmdTitleBlock (Object hashmap) =
  vcat $ map go $ sortBy (comparing fst) $ H.toList hashmap
  where go (k,v) =
          case (text (T.unpack k), v) of
               (k', Array vec)
                 | V.null vec     -> empty
                 | otherwise      -> k' <> ":" <> space <>
                                      hcat (intersperse "; "
                                           (map fromstr $ V.toList vec))
               (_, String "")  -> empty
               (k', x)         -> k' <> ":" <> space <> nest 2 (fromstr x)
        fromstr (String s) = text (removeBlankLines $ T.unpack s)
        fromstr (Bool b)   = text (show b)
        fromstr (Number n) = text (show n)
        fromstr _          = empty
        -- blank lines not allowed in MMD metadata - we replace with .
        removeBlankLines   = trimr . unlines . map (\x ->
                               if all isSpace x then "." else x) . lines
mmdTitleBlock _ = empty

plainTitleBlock :: Doc -> [Doc] -> Doc -> Doc
plainTitleBlock tit auths dat =
  tit <> cr <>
  (hcat (intersperse (text "; ") auths)) <> cr <>
  dat <> cr

yamlMetadataBlock :: Value -> Doc
yamlMetadataBlock v = "---" $$ (jsonToYaml v) $$ "..."

jsonToYaml :: Value -> Doc
jsonToYaml (Object hashmap) =
  vcat $ map (\(k,v) ->
          case (text (T.unpack k), v, jsonToYaml v) of
               (k', Array vec, x)
                 | V.null vec     -> empty
                 | otherwise      -> (k' <> ":") $$ x
               (k', Object _, x)  -> (k' <> ":") $$ nest 2 x
               (_, String "", _)  -> empty
               (k', _, x)         -> k' <> ":" <> space <> hang 2 "" x)
       $ sortBy (comparing fst) $ H.toList hashmap
jsonToYaml (Array vec) =
  vcat $ map (\v -> hang 2 "- " (jsonToYaml v)) $ V.toList vec
jsonToYaml (String "") = empty
jsonToYaml (String s) =
  case T.unpack s of
     x | '\n' `elem` x -> hang 2 ("|" <> cr) $ text x
       | not (any isPunctuation x) -> text x
       | otherwise     -> text $ "'" ++ substitute "'" "''" x ++ "'"
jsonToYaml (Bool b) = text $ show b
jsonToYaml (Number n) = text $ show n
jsonToYaml _ = empty

-- | Return markdown representation of document.
pandocToMarkdown :: WriterOptions -> Pandoc -> State WriterState String
pandocToMarkdown opts (Pandoc meta blocks) = do
  let colwidth = if writerWrapText opts
                    then Just $ writerColumns opts
                    else Nothing
  isPlain <- gets stPlain
  metadata <- metaToJSON opts
               (fmap (render colwidth) . blockListToMarkdown opts)
               (fmap (render colwidth) . inlineListToMarkdown opts)
               meta
  let title' = maybe empty text $ getField "title" metadata
  let authors' = maybe [] (map text) $ getField "author" metadata
  let date' = maybe empty text $ getField "date" metadata
  let titleblock = case writerStandalone opts of
                        True | isPlain ->
                                plainTitleBlock title' authors' date'
                             | isEnabled Ext_yaml_metadata_block opts ->
                                 yamlMetadataBlock metadata
                             | isEnabled Ext_pandoc_title_block opts ->
                                 pandocTitleBlock title' authors' date'
                             | isEnabled Ext_mmd_title_block opts ->
                                 mmdTitleBlock metadata
                             | otherwise -> empty
                        False -> empty
  let headerBlocks = filter isHeaderBlock blocks
  let toc = if writerTableOfContents opts
               then tableOfContents opts headerBlocks
               else empty
  -- Strip off final 'references' header if markdown citations enabled
  let blocks' = if not isPlain && isEnabled Ext_citations opts
                   then case reverse blocks of
                             (Div (_,["references"],_) _):xs -> reverse xs
                             _ -> blocks
                   else blocks
  body <- blockListToMarkdown opts blocks'
  st <- get
  notes' <- notesToMarkdown opts (reverse $ stNotes st)
  st' <- get  -- note that the notes may contain refs
  refs' <- refsToMarkdown opts (reverse $ stRefs st')
  let render' :: Doc -> String
      render' = render colwidth
  let main = render' $ body <>
               (if isEmpty notes' then empty else blankline <> notes') <>
               (if isEmpty refs' then empty else blankline <> refs')
  let context  = defField "toc" (render' toc)
               $ defField "body" main
               $ (if isNullMeta meta
                     then id
                     else defField "titleblock" (render' titleblock))
               $ metadata
  if writerStandalone opts
     then return $ renderTemplate' (writerTemplate opts) context
     else return main

-- | Return markdown representation of reference key table.
refsToMarkdown :: WriterOptions -> Refs -> State WriterState Doc
refsToMarkdown opts refs = mapM (keyToMarkdown opts) refs >>= return . vcat

-- | Return markdown representation of a reference key.
keyToMarkdown :: WriterOptions
              -> ([Inline], (String, String))
              -> State WriterState Doc
keyToMarkdown opts (label, (src, tit)) = do
  label' <- inlineListToMarkdown opts label
  let tit' = if null tit
                then empty
                else space <> "\"" <> text tit <> "\""
  return $ nest 2 $ hang 2
            ("[" <> label' <> "]:" <> space) (text src <> tit')

-- | Return markdown representation of notes.
notesToMarkdown :: WriterOptions -> [[Block]] -> State WriterState Doc
notesToMarkdown opts notes =
  mapM (\(num, note) -> noteToMarkdown opts num note) (zip [1..] notes) >>=
  return . vsep

-- | Return markdown representation of a note.
noteToMarkdown :: WriterOptions -> Int -> [Block] -> State WriterState Doc
noteToMarkdown opts num blocks = do
  contents  <- blockListToMarkdown opts blocks
  let num' = text $ writerIdentifierPrefix opts ++ show num
  let marker = if isEnabled Ext_footnotes opts
                  then text "[^" <> num' <> text "]:"
                  else text "[" <> num' <> text "]"
  let markerSize = 4 + offset num'
  let spacer = case writerTabStop opts - markerSize of
                     n | n > 0  -> text $ replicate n ' '
                     _          -> text " "
  return $ if isEnabled Ext_footnotes opts
              then hang (writerTabStop opts) (marker <> spacer) contents
              else marker <> spacer <> contents

-- | Escape special characters for Markdown.
escapeString :: String -> String
escapeString = escapeStringUsing markdownEscapes
  where markdownEscapes = backslashEscapes "\\`*_$<>#~^"

-- | Construct table of contents from list of header blocks.
tableOfContents :: WriterOptions -> [Block] -> Doc
tableOfContents opts headers =
  let opts' = opts { writerIgnoreNotes = True }
      contents = BulletList $ map (elementToListItem opts) $ hierarchicalize headers
  in  evalState (blockToMarkdown opts' contents) def

-- | Converts an Element to a list item for a table of contents,
elementToListItem :: WriterOptions -> Element -> [Block]
elementToListItem opts (Sec lev _ _ headerText subsecs)
  = Plain headerText :
    [ BulletList (map (elementToListItem opts) subsecs) |
      not (null subsecs) && lev < writerTOCDepth opts ]
elementToListItem _ (Blk _) = []

attrsToMarkdown :: Attr -> Doc
attrsToMarkdown attribs = braces $ hsep [attribId, attribClasses, attribKeys]
        where attribId = case attribs of
                                ([],_,_) -> empty
                                (i,_,_)  -> "#" <> text i
              attribClasses = case attribs of
                                (_,[],_) -> empty
                                (_,cs,_) -> hsep $
                                            map (text . ('.':))
                                            cs
              attribKeys = case attribs of
                                (_,_,[]) -> empty
                                (_,_,ks) -> hsep $
                                            map (\(k,v) -> text k
                                              <> "=\"" <> text v <> "\"") ks

-- | Ordered list start parser for use in Para below.
olMarker :: Parser [Char] ParserState Char
olMarker = do (start, style', delim) <- anyOrderedListMarker
              if delim == Period &&
                          (style' == UpperAlpha || (style' == UpperRoman &&
                          start `elem` [1, 5, 10, 50, 100, 500, 1000]))
                          then spaceChar >> spaceChar
                          else spaceChar

-- | True if string begins with an ordered list marker
beginsWithOrderedListMarker :: String -> Bool
beginsWithOrderedListMarker str =
  case runParser olMarker defaultParserState "para start" (take 10 str) of
         Left  _  -> False
         Right _  -> True

-- | Convert Pandoc block element to markdown.
blockToMarkdown :: WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> State WriterState Doc
blockToMarkdown _ Null = return empty
blockToMarkdown opts (Div attrs ils) = do
  isPlain <- gets stPlain
  contents <- blockListToMarkdown opts ils
  return $ if isPlain || not (isEnabled Ext_markdown_in_html_blocks opts)
              then contents <> blankline
              else tagWithAttrs "div" attrs <> blankline <>
                      contents <> blankline <> "</div>" <> blankline
blockToMarkdown opts (Plain inlines) = do
  contents <- inlineListToMarkdown opts inlines
  return $ contents <> cr
-- title beginning with fig: indicates figure
blockToMarkdown opts (Para [Image alt (src,'f':'i':'g':':':tit)]) =
  blockToMarkdown opts (Para [Image alt (src,tit)])
blockToMarkdown opts (Para inlines) = do
  contents <- inlineListToMarkdown opts inlines
  -- escape if para starts with ordered list marker
  st <- get
  let esc = if isEnabled Ext_all_symbols_escapable opts &&
               not (stPlain st) &&
               beginsWithOrderedListMarker (render Nothing contents)
               then text "\x200B" -- zero-width space, a hack
               else empty
  return $ esc <> contents <> blankline
blockToMarkdown opts (RawBlock f str)
  | f == "html" = do
    st <- get
    if stPlain st
       then return empty
       else return $ if isEnabled Ext_markdown_attribute opts
                        then text (addMarkdownAttribute str) <> text "\n"
                        else text str <> text "\n"
  | f `elem` ["latex", "tex", "markdown"] = do
    st <- get
    if stPlain st
       then return empty
       else return $ text str <> text "\n"
blockToMarkdown _ (RawBlock _ _) = return empty
blockToMarkdown _ HorizontalRule =
  return $ blankline <> text "* * * * *" <> blankline
blockToMarkdown opts (Header level attr inlines) = do
  -- we calculate the id that would be used by auto_identifiers
  -- so we know whether to print an explicit identifier
  ids <- gets stIds
  let autoId = uniqueIdent inlines ids
  modify $ \st -> st{ stIds = autoId : ids }
  let attr' = case attr of
                   ("",[],[]) -> empty
                   (id',[],[]) | isEnabled Ext_auto_identifiers opts
                                 && id' == autoId -> empty
                   (id',_,_)   | isEnabled Ext_mmd_header_identifiers opts ->
                                    space <> brackets (text id')
                   _ | isEnabled Ext_header_attributes opts ->
                                    space <> attrsToMarkdown attr
                     | otherwise -> empty
  contents <- inlineListToMarkdown opts inlines
  st <- get
  let setext = writerSetextHeaders opts
  return $ nowrap
         $ case level of
            1 | setext ->
                  contents <> attr' <> cr <> text (replicate (offset contents) '=') <>
                  blankline
            2 | setext ->
                  contents <> attr' <> cr <> text (replicate (offset contents) '-') <>
                  blankline
            -- ghc interprets '#' characters in column 1 as linenum specifiers.
            _ | stPlain st || isEnabled Ext_literate_haskell opts ->
                contents <> blankline
            _ -> text (replicate level '#') <> space <> contents <> attr' <> blankline
blockToMarkdown opts (CodeBlock (_,classes,_) str)
  | "haskell" `elem` classes && "literate" `elem` classes &&
    isEnabled Ext_literate_haskell opts =
  return $ prefixed "> " (text str) <> blankline
blockToMarkdown opts (CodeBlock attribs str) = return $
  case attribs == nullAttr of
     False | isEnabled Ext_backtick_code_blocks opts ->
          backticks <> attrs <> cr <> text str <> cr <> backticks <> blankline
           | isEnabled Ext_fenced_code_blocks opts ->
          tildes <> attrs <> cr <> text str <> cr <> tildes <> blankline
     _ -> nest (writerTabStop opts) (text str) <> blankline
   where tildes    = text $ case [ln | ln <- lines str, all (=='~') ln] of
                               [] -> "~~~~"
                               xs -> case maximum $ map length xs of
                                          n | n < 3 -> "~~~~"
                                            | otherwise -> replicate (n+1) '~'
         backticks = text "```"
         attrs  = if isEnabled Ext_fenced_code_attributes opts
                     then nowrap $ " " <> attrsToMarkdown attribs
                     else case attribs of
                                (_,[cls],_) -> " " <> text cls
                                _           -> empty
blockToMarkdown opts (BlockQuote blocks) = do
  st <- get
  -- if we're writing literate haskell, put a space before the bird tracks
  -- so they won't be interpreted as lhs...
  let leader = if isEnabled Ext_literate_haskell opts
                  then " > "
                  else if stPlain st
                          then "  "
                          else "> "
  contents <- blockListToMarkdown opts blocks
  return $ (prefixed leader contents) <> blankline
blockToMarkdown opts t@(Table caption aligns widths headers rows) =  do
  caption' <- inlineListToMarkdown opts caption
  let caption'' = if null caption || not (isEnabled Ext_table_captions opts)
                     then empty
                     else blankline <> ": " <> caption' <> blankline
  rawHeaders <- mapM (blockListToMarkdown opts) headers
  rawRows <- mapM (mapM (blockListToMarkdown opts)) rows
  let isSimple = all (==0) widths
  let isPlainBlock (Plain _) = True
      isPlainBlock _         = False
  let hasBlocks = not (all isPlainBlock $ concat . concat $ headers:rows)
  (nst,tbl) <- case True of
                _ | isSimple &&
                    isEnabled Ext_simple_tables opts -> fmap (nest 2,) $
                         pandocTable opts (all null headers) aligns widths
                             rawHeaders rawRows
                  | isSimple &&
                    isEnabled Ext_pipe_tables opts -> fmap (id,) $
                         pipeTable (all null headers) aligns rawHeaders rawRows
                  | not hasBlocks &&
                    isEnabled Ext_multiline_tables opts -> fmap (nest 2,) $
                         pandocTable opts (all null headers) aligns widths
                             rawHeaders rawRows
                  | isEnabled Ext_grid_tables opts -> fmap (id,) $
                         gridTable opts (all null headers) aligns widths
                             rawHeaders rawRows
                  | otherwise -> fmap (id,) $
                         return $ text $ writeHtmlString def
                                $ Pandoc nullMeta [t]
  return $ nst $ tbl $$ blankline $$ caption'' $$ blankline
blockToMarkdown opts (BulletList items) = do
  contents <- mapM (bulletListItemToMarkdown opts) items
  return $ cat contents <> blankline
blockToMarkdown opts (OrderedList (start,sty,delim) items) = do
  let start' = if isEnabled Ext_startnum opts then start else 1
  let sty'   = if isEnabled Ext_fancy_lists opts then sty else DefaultStyle
  let delim' = if isEnabled Ext_fancy_lists opts then delim else DefaultDelim
  let attribs = (start', sty', delim')
  let markers  = orderedListMarkers attribs
  let markers' = map (\m -> if length m < 3
                               then m ++ replicate (3 - length m) ' '
                               else m) markers
  contents <- mapM (\(item, num) -> orderedListItemToMarkdown opts item num) $
              zip markers' items
  return $ cat contents <> blankline
blockToMarkdown opts (DefinitionList items) = do
  contents <- mapM (definitionListItemToMarkdown opts) items
  return $ cat contents <> blankline

addMarkdownAttribute :: String -> String
addMarkdownAttribute s =
  case span isTagText $ reverse $ parseTags s of
       (xs,(TagOpen t attrs:rest)) ->
            renderTags $ reverse rest ++ (TagOpen t attrs' : reverse xs)
              where attrs' = ("markdown","1"):[(x,y) | (x,y) <- attrs,
                                 x /= "markdown"]
       _ -> s

pipeTable :: Bool -> [Alignment] -> [Doc] -> [[Doc]] -> State WriterState Doc
pipeTable headless aligns rawHeaders rawRows = do
  let torow cs = nowrap $ text "|" <>
                 hcat (intersperse (text "|") $ map chomp cs) <> text "|"
  let toborder (a, h) = let wid = max (offset h) 3
                        in  text $ case a of
                             AlignLeft    -> ':':replicate (wid - 1) '-'
                             AlignCenter  -> ':':replicate (wid - 2) '-' ++ ":"
                             AlignRight   -> replicate (wid - 1) '-' ++ ":"
                             AlignDefault -> replicate wid '-'
  let header = if headless then empty else torow rawHeaders
  let border = torow $ map toborder $ zip aligns rawHeaders
  let body   = vcat $ map torow rawRows
  return $ header $$ border $$ body

pandocTable :: WriterOptions -> Bool -> [Alignment] -> [Double]
            -> [Doc] -> [[Doc]] -> State WriterState Doc
pandocTable opts headless aligns widths rawHeaders rawRows =  do
  let isSimple = all (==0) widths
  let alignHeader alignment = case alignment of
                                AlignLeft    -> lblock
                                AlignCenter  -> cblock
                                AlignRight   -> rblock
                                AlignDefault -> lblock
  let numChars = maximum . map offset
  let widthsInChars = if isSimple
                         then map ((+2) . numChars)
                              $ transpose (rawHeaders : rawRows)
                         else map
                              (floor . (fromIntegral (writerColumns opts) *))
                              widths
  let makeRow = hcat . intersperse (lblock 1 (text " ")) .
                   (zipWith3 alignHeader aligns widthsInChars)
  let rows' = map makeRow rawRows
  let head' = makeRow rawHeaders
  let maxRowHeight = maximum $ map height (head':rows')
  let underline = cat $ intersperse (text " ") $
                  map (\width -> text (replicate width '-')) widthsInChars
  let border = if maxRowHeight > 1
                  then text (replicate (sum widthsInChars +
                          length widthsInChars - 1) '-')
                  else if headless
                          then underline
                          else empty
  let head'' = if headless
                  then empty
                  else border <> cr <> head'
  let body = if maxRowHeight > 1
                then vsep rows'
                else vcat rows'
  let bottom = if headless
                  then underline
                  else border
  return $ head'' $$ underline $$ body $$ bottom

gridTable :: WriterOptions -> Bool -> [Alignment] -> [Double]
          -> [Doc] -> [[Doc]] -> State WriterState Doc
gridTable opts headless _aligns widths headers' rawRows =  do
  let numcols = length headers'
  let widths' = if all (==0) widths
                   then replicate numcols (1.0 / fromIntegral numcols)
                   else widths
  let widthsInChars = map (floor . (fromIntegral (writerColumns opts) *)) widths'
  let hpipeBlocks blocks = hcat [beg, middle, end]
        where h       = maximum (map height blocks)
              sep'    = lblock 3 $ vcat (map text $ replicate h " | ")
              beg     = lblock 2 $ vcat (map text $ replicate h "| ")
              end     = lblock 2 $ vcat (map text $ replicate h " |")
              middle  = chomp $ hcat $ intersperse sep' blocks
  let makeRow = hpipeBlocks . zipWith lblock widthsInChars
  let head' = makeRow headers'
  let rows' = map (makeRow . map chomp) rawRows
  let border ch = char '+' <> char ch <>
                  (hcat $ intersperse (char ch <> char '+' <> char ch) $
                          map (\l -> text $ replicate l ch) widthsInChars) <>
                  char ch <> char '+'
  let body = vcat $ intersperse (border '-') rows'
  let head'' = if headless
                  then empty
                  else head' $$ border '='
  return $ border '-' $$ head'' $$ body $$ border '-'

-- | Convert bullet list item (list of blocks) to markdown.
bulletListItemToMarkdown :: WriterOptions -> [Block] -> State WriterState Doc
bulletListItemToMarkdown opts items = do
  contents <- blockListToMarkdown opts items
  let sps = replicate (writerTabStop opts - 2) ' '
  let start = text ('-' : ' ' : sps)
  -- remove trailing blank line if it is a tight list
  let contents' = case reverse items of
                       (BulletList xs:_) | isTightList xs ->
                            chomp contents <> cr
                       (OrderedList _ xs:_) | isTightList xs ->
                            chomp contents <> cr
                       _ -> contents
  return $ hang (writerTabStop opts) start $ contents' <> cr

-- | Convert ordered list item (a list of blocks) to markdown.
orderedListItemToMarkdown :: WriterOptions -- ^ options
                          -> String        -- ^ list item marker
                          -> [Block]       -- ^ list item (list of blocks)
                          -> State WriterState Doc
orderedListItemToMarkdown opts marker items = do
  contents <- blockListToMarkdown opts items
  let sps = case length marker - writerTabStop opts of
                   n | n > 0 -> text $ replicate n ' '
                   _         -> text " "
  let start = text marker <> sps
  return $ hang (writerTabStop opts) start $ contents <> cr

-- | Convert definition list item (label, list of blocks) to markdown.
definitionListItemToMarkdown :: WriterOptions
                             -> ([Inline],[[Block]])
                             -> State WriterState Doc
definitionListItemToMarkdown opts (label, defs) = do
  labelText <- inlineListToMarkdown opts label
  defs' <- mapM (mapM (blockToMarkdown opts)) defs
  if isEnabled Ext_definition_lists opts
     then do
       let tabStop = writerTabStop opts
       st <- get
       let leader  = if stPlain st then "   " else ":  "
       let sps = case writerTabStop opts - 3 of
                      n | n > 0   -> text $ replicate n ' '
                      _           -> text " "
       let contents = vcat $ map (\d -> hang tabStop (leader <> sps) $ vcat d <> cr) defs'
       return $ nowrap labelText <> cr <> contents <> cr
     else do
       return $ nowrap labelText <> text "  " <> cr <>
                vsep (map vsep defs') <> blankline

-- | Convert list of Pandoc block elements to markdown.
blockListToMarkdown :: WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> State WriterState Doc
blockListToMarkdown opts blocks =
  mapM (blockToMarkdown opts) (fixBlocks blocks) >>= return . cat
    -- insert comment between list and indented code block, or the
    -- code block will be treated as a list continuation paragraph
    where fixBlocks (b : CodeBlock attr x : rest)
            | (not (isEnabled Ext_fenced_code_blocks opts) || attr == nullAttr)
                && isListBlock b =
               b : RawBlock "html" "<!-- -->\n" : CodeBlock attr x :
                   fixBlocks rest
          fixBlocks (x : xs)             = x : fixBlocks xs
          fixBlocks []                   = []
          isListBlock (BulletList _)     = True
          isListBlock (OrderedList _ _)  = True
          isListBlock (DefinitionList _) = True
          isListBlock _                  = False

-- | Get reference for target; if none exists, create unique one and return.
--   Prefer label if possible; otherwise, generate a unique key.
getReference :: [Inline] -> Target -> State WriterState [Inline]
getReference label (src, tit) = do
  st <- get
  case find ((== (src, tit)) . snd) (stRefs st) of
    Just (ref, _) -> return ref
    Nothing       -> do
      let label' = case find ((== label) . fst) (stRefs st) of
                      Just _ -> -- label is used; generate numerical label
                             case find (\n -> notElem [Str (show n)]
                                                      (map fst (stRefs st)))
                                       [1..(10000 :: Integer)] of
                                  Just x  -> [Str (show x)]
                                  Nothing -> error "no unique label"
                      Nothing -> label
      modify (\s -> s{ stRefs = (label', (src,tit)) : stRefs st })
      return label'

-- | Convert list of Pandoc inline elements to markdown.
inlineListToMarkdown :: WriterOptions -> [Inline] -> State WriterState Doc
inlineListToMarkdown opts lst =
  mapM (inlineToMarkdown opts) lst >>= return . cat

escapeSpaces :: Inline -> Inline
escapeSpaces (Str s) = Str $ substitute " " "\\ " s
escapeSpaces Space = Str "\\ "
escapeSpaces x = x

-- | Convert Pandoc inline element to markdown.
inlineToMarkdown :: WriterOptions -> Inline -> State WriterState Doc
inlineToMarkdown opts (Span attrs ils) = do
  st <- get
  contents <- inlineListToMarkdown opts ils
  return $ if stPlain st
              then contents
              else tagWithAttrs "span" attrs <> contents <> text "</span>"
inlineToMarkdown opts (Emph lst) = do
  contents <- inlineListToMarkdown opts lst
  return $ "*" <> contents <> "*"
inlineToMarkdown opts (Strong lst) = do
  contents <- inlineListToMarkdown opts lst
  return $ "**" <> contents <> "**"
inlineToMarkdown opts (Strikeout lst) = do
  contents <- inlineListToMarkdown opts lst
  return $ if isEnabled Ext_strikeout opts
              then "~~" <> contents <> "~~"
              else "<s>" <> contents <> "</s>"
inlineToMarkdown opts (Superscript lst) = do
  let lst' = walk escapeSpaces lst
  contents <- inlineListToMarkdown opts lst'
  return $ if isEnabled Ext_superscript opts
              then "^" <> contents <> "^"
              else "<sup>" <> contents <> "</sup>"
inlineToMarkdown opts (Subscript lst) = do
  let lst' = walk escapeSpaces lst
  contents <- inlineListToMarkdown opts lst'
  return $ if isEnabled Ext_subscript opts
              then "~" <> contents <> "~"
              else "<sub>" <> contents <> "</sub>"
inlineToMarkdown opts (SmallCaps lst) = inlineListToMarkdown opts lst
inlineToMarkdown opts (Quoted SingleQuote lst) = do
  contents <- inlineListToMarkdown opts lst
  return $ "‘" <> contents <> "’"
inlineToMarkdown opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToMarkdown opts lst
  return $ "“" <> contents <> "”"
inlineToMarkdown opts (Code attr str) =
  let tickGroups = filter (\s -> '`' `elem` s) $ group str
      longest    = if null tickGroups
                     then 0
                     else maximum $ map length tickGroups
      marker     = replicate (longest + 1) '`'
      spacer     = if (longest == 0) then "" else " "
      attrs      = if isEnabled Ext_inline_code_attributes opts && attr /= nullAttr
                      then attrsToMarkdown attr
                      else empty
  in  return $ text (marker ++ spacer ++ str ++ spacer ++ marker) <> attrs
inlineToMarkdown _ (Str str) = do
  st <- get
  if stPlain st
     then return $ text str
     else return $ text $ escapeString str
inlineToMarkdown opts (Math InlineMath str)
  | isEnabled Ext_tex_math_dollars opts =
      return $ "$" <> text str <> "$"
  | isEnabled Ext_tex_math_single_backslash opts =
      return $ "\\(" <> text str <> "\\)"
  | isEnabled Ext_tex_math_double_backslash opts =
      return $ "\\\\(" <> text str <> "\\\\)"
  | isEnabled Ext_scholarly_markdown opts =
      return $ "``" <> text (trim str) <> "``"
  | otherwise = inlineListToMarkdown opts $ readTeXMath' InlineMath str
inlineToMarkdown opts (Math (DisplayMath a) str)
  | isEnabled Ext_tex_math_dollars opts =
      return $ "$$" <> text str <> "$$"
  | isEnabled Ext_tex_math_single_backslash opts =
      return $ "\\[" <> text str <> "\\]"
  | isEnabled Ext_tex_math_double_backslash opts =
      return $ "\\\\[" <> text str <> "\\\\]"
  | otherwise = (\x -> cr <> x <> cr) `fmap`
        inlineListToMarkdown opts (readTeXMath' (DisplayMath a) str)
inlineToMarkdown opts (RawInline f str)
  | f == "html" || f == "markdown" ||
    (isEnabled Ext_raw_tex opts && (f == "latex" || f == "tex")) =
    return $ text str
inlineToMarkdown _ (RawInline _ _) = return empty
inlineToMarkdown opts (LineBreak)
  | isEnabled Ext_hard_line_breaks opts    = return cr
  | isEnabled Ext_escaped_line_breaks opts = return $ "\\" <> cr
  | otherwise                              = return $ "  " <> cr
inlineToMarkdown _ Space = return space
inlineToMarkdown opts (Cite [] lst) = inlineListToMarkdown opts lst
inlineToMarkdown opts (Cite (c:cs) lst)
  | not (isEnabled Ext_citations opts) = inlineListToMarkdown opts lst
  | otherwise =
      if citationMode c == AuthorInText
         then do
           suffs <- inlineListToMarkdown opts $ citationSuffix c
           rest <- mapM convertOne cs
           let inbr = suffs <+> joincits rest
               br   = if isEmpty inbr then empty else char '[' <> inbr <> char ']'
           return $ text ("@" ++ citationId c) <+> br
         else do
           cits <- mapM convertOne (c:cs)
           return $ text "[" <> joincits cits <> text "]"
  where
        joincits = hcat . intersperse (text "; ") . filter (not . isEmpty)
        convertOne Citation { citationId      = k
                            , citationPrefix  = pinlines
                            , citationSuffix  = sinlines
                            , citationMode    = m }
                               = do
           pdoc <- inlineListToMarkdown opts pinlines
           sdoc <- inlineListToMarkdown opts sinlines
           let k' = text (modekey m ++ "@" ++ k)
               r = case sinlines of
                        Str (y:_):_ | y `elem` ",;]@" -> k' <> sdoc
                        _                             -> k' <+> sdoc
           return $ pdoc <+> r
        modekey SuppressAuthor = "-"
        modekey _              = ""
inlineToMarkdown opts (Link txt (src, tit)) = do
  linktext <- inlineListToMarkdown opts txt
  let linktitle = if null tit
                     then empty
                     else text $ " \"" ++ tit ++ "\""
  let srcSuffix = if isPrefixOf "mailto:" src then drop 7 src else src
  let useAuto = isURI src &&
                case txt of
                      [Str s] | escapeURI s == srcSuffix -> True
                      _                                  -> False
  let useRefLinks = writerReferenceLinks opts && not useAuto
  ref <- if useRefLinks then getReference txt (src, tit) else return []
  reftext <- inlineListToMarkdown opts ref
  return $ if useAuto
              then "<" <> text srcSuffix <> ">"
              else if useRefLinks
                      then let first  = "[" <> linktext <> "]"
                               second = if txt == ref
                                           then "[]"
                                           else "[" <> reftext <> "]"
                           in  first <> second
                      else "[" <> linktext <> "](" <>
                           text src <> linktitle <> ")"
inlineToMarkdown opts (Image alternate (source, tit)) = do
  let txt = if null alternate || alternate == [Str source]
                                 -- to prevent autolinks
               then [Str ""]
               else alternate
  linkPart <- inlineToMarkdown opts (Link txt (source, tit))
  return $ "!" <> linkPart
inlineToMarkdown opts (Note contents) = do
  modify (\st -> st{ stNotes = contents : stNotes st })
  st <- get
  let ref = text $ writerIdentifierPrefix opts ++ show (length $ stNotes st)
  if isEnabled Ext_footnotes opts
     then return $ "[^" <> ref <> "]"
     else return $ "[" <> ref <> "]"

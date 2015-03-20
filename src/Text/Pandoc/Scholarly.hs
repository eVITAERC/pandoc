{-
Copyright (C) 2014 Tim T.Y. Lin <timtylin@gmail.com>

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
   Module      : Text.Pandoc.Scholarly
   Copyright   : Copyright (C) 2014 Tim T.Y. Lin
   License     : GNU GPL, version 2 or above

   Maintainer  : Tim T.Y. Lin <timtylin@gmail.com>
   Stability   : alpha
   Portability : portable

Utility functions for Scholarly Markdown extensions.
-}
module Text.Pandoc.Scholarly (classIsMath,
                              classIsMathDef,
                              processSingleEqn,
                              processMultiEqn,
                              dispMathToLaTeX,
                              figureIdToNumLabelHandler,
                              AttributedMath,
                              getImageAttr,
                              getIdentifier,
                              getClasses,
                              hasClass,
                              getKeyVals,
                              lookupKey,
                              setIdentifier,
                              insertClass,
                              insertIfNoneKeyVal,
                              insertReplaceKeyVal,
                              insertReplaceKeyValIf,
                              extractMetaStringList,
                              extractMetaString
                             )
where

import Data.List ( intercalate )
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.Parsing hiding (tableWith)
import Control.Arrow
import Control.Monad (void)
import qualified Data.Map as M

type AttributedMath = (Attr, String)

-- true only if some element of classes start with "math"
classIsMath :: Attr -> Bool
classIsMath (_,classes,_) = any (`elem` ["math", "math_def", "math_plain"]) classes

classIsMathDef :: Attr -> Bool
classIsMathDef (_,classes,_) = "math_def" `elem` classes

--
-- Attribute manipulation functions
--

insertClass :: String -> Attr -> Attr
insertClass className attr@(ident, classes, keyval)
  | className `notElem` classes = (ident, className:classes, keyval)
  | otherwise = attr

insertWithKeyVal :: (String -> String -> String) -- ^ new, old, final value
                 -> (String, String) -- ^ (key, new value)
                 -> Attr
                 -> Attr
insertWithKeyVal f (key, val) (ident, classes, keyval) =
  let newKeyValMap = M.insertWith f key val $ M.fromList keyval
  in (ident, classes, M.toList newKeyValMap)

insertIfNoneKeyVal :: (String, String) -> Attr -> Attr
insertIfNoneKeyVal = insertWithKeyVal (\_ x -> x)

insertReplaceKeyVal :: (String, String) -> Attr -> Attr
insertReplaceKeyVal = insertWithKeyVal const

insertReplaceKeyValIf :: Bool -> (String, String) -> Attr -> Attr
insertReplaceKeyValIf True kv attr = insertReplaceKeyVal kv attr
insertReplaceKeyValIf False _ attr = attr

getClasses :: Attr -> [String]
getClasses (_, classes, _) = classes

hasClass :: String -> Attr -> Bool
hasClass cls (_, classes, _) = cls `elem` classes

getIdentifier :: Attr -> String
getIdentifier (identifier, _, _) = identifier

setIdentifier :: String -> Attr -> Attr
setIdentifier identifier (_, classes, keyval) = (identifier, classes, keyval)

getKeyVals :: Attr -> [(String, String)]
getKeyVals (_, _, keyVals) = keyVals

lookupKey :: String -> Attr -> Maybe String
lookupKey key (_, _, keyval) = M.lookup key $ M.fromList keyval

getImageAttr :: Inline -> Attr
getImageAttr (Image attr _ _) = attr
getImageAttr _ = nullAttr

--
-- Writer state helpers (useful for cross-references)
--

extractMetaStringList :: Maybe MetaValue -> [String]
extractMetaStringList (Just (MetaList lst)) = map extractMetaString lst
extractMetaStringList (Just (MetaString str)) = [str]
extractMetaStringList _ = []

extractMetaString :: MetaValue -> String
extractMetaString (MetaString str) = str
extractMetaString _ = ""

---
--- Parser functions for Scholarly DisplayMath
---

-- Currently does the following:
-- 1) automatically wrap in @aligned@ or @split@ envionrment if needed
-- 2) if attribute has id, append @\label{id}@ to code
-- 3) Returns also the label string in an list
processSingleEqn :: AttributedMath -> (AttributedMath, [String])
processSingleEqn eqn =
  let processors = [appendLabel "\n", -- ensureNonumber is handled by writer
                    ensureMultilineEnv]
      label = (getIdentifier . fst) eqn
  in (foldr ($) eqn processors, [label])

-- Currently does the following:
-- 1) trim whitespace from all equation codes
-- 2) if attribute has id, prepend @\label{id}@ to code
-- 3) if attribute has no id, prepend @\nonumber@ to code
-- 4) concatenate all equations into one code chunk delimited by @'\\'@
-- 5) assign @align@ or @gather@ class as needed
-- 6) gather all equation labels as list and output to @labelList@ key
processMultiEqn :: [AttributedMath] -> (AttributedMath, [String])
processMultiEqn eqnList =
  let processors = [ensureNonumber " ",
                    prependLabel " ",
                    second trim]
      processedEqnList = foldr map eqnList processors
      labels = map (getIdentifier . fst) eqnList
  in (concatMultiEquations processedEqnList, labels)

-- Automatically surround with split env if naked token @'\\'@ detected,
-- or aligned env if both naked token @'\\'@ and @'&'@ detected.
-- Skipped classes: [math_plain]
ensureMultilineEnv :: AttributedMath -> AttributedMath
ensureMultilineEnv eqn@(attr, content)
  | "math_plain" `elem` getClasses attr = eqn
  | hasTeXLinebreak content = if hasTeXAlignment content
                                 then (attr, wrapInLatexEnv "aligned" content)
                                 else (attr, wrapInLatexEnv "split" content)
  | otherwise = eqn

-- if attribute has no id, append @\nonumber@ to code
ensureNonumber :: String -> AttributedMath -> AttributedMath
ensureNonumber terminal eqn@(attr, content) =
  case attr of
    ("",_ ,_) -> (attr, "\\nonumber" ++ terminal ++ content)
    _         -> eqn

-- if attribute has id, prepend @\label{id}@ to code
-- (does not ensure no duplicate labels)
prependLabel :: String -> AttributedMath -> AttributedMath
prependLabel terminal eqn@(attr, content) =
  case attr of
    ("",_ ,_)     -> eqn
    (label, _, _) -> (attr, "\\label{" ++ label ++ "}" ++ terminal ++ content )

-- if attribute has id, append @\label{id}@ to code
-- (does not ensure no duplicate labels)
-- This function is needed due to MathJax bug 1020
appendLabel :: String -> AttributedMath -> AttributedMath
appendLabel terminal eqn@(attr, content) =
  case attr of
    ("",_ ,_)     -> eqn
    (label, _, _) -> (attr, content ++ terminal ++ "\\label{" ++ label ++ "}")

-- scans first equation for alignment characters,
-- assign @align@ or @gather@ accordingly,
-- then concatenate all lines into one multi-equation displayMath,
-- gathering the idents of all equations into one large list
concatMultiEquations :: [AttributedMath] -> AttributedMath
concatMultiEquations eqnList =
  let eqnContents = map snd eqnList
      multiClass = if not $ any hasTeXAlignment eqnContents
                      then "gather"
                      else "align"
  in ( ("", ["math",multiClass], [("labelList",show (map (getIdentifier.fst) eqnList))]),
       intercalate "\\\\\n" eqnContents )

wrapInLatexEnv :: String -> String -> String
wrapInLatexEnv envName content = intercalate "\n"
            ["\\begin{" ++ envName ++ "}", content, "\\end{" ++ envName ++ "}"]

-- Scan for occurance of @'\\'@ in non-commented parts,
-- not within "split" or "aligned" environment
hasTeXLinebreak :: String -> Bool
hasTeXLinebreak content =
  case parse (skipMany (try skipMultilineDetection
                        <|> try (char '\\' >> notFollowedBy (char '\\') >> return [])
                        <|> try (noneOf "\\" >> return []))
               >> void (string "\\\\")) [] content of
       Left _  -> False
       Right _ -> True

-- Scan for occurance of non-escaped @'&'@ in non-commented parts
-- not within "split" or "aligned" environment
hasTeXAlignment :: String -> Bool
hasTeXAlignment content =
  case parse (skipMany (try skipMultilineDetection
                        <|>  try (string "\\&")
                        <|>  try (noneOf "&" >> return []))
              >> void (char '&')) [] content of
       Left _  -> False
       Right _ -> True

skipTeXComment :: Parser String st String
skipTeXComment = try $ do
  char '%'
  manyTill anyChar $ try $ newline <|> (eof >> return '\n')
  return []

skipTexEnvironment :: String -> Parser String st String
skipTexEnvironment envName = try $ do
  string ("\\begin{" ++ envName ++ "}")
  manyTill anyChar $ try $ string ("\\end{" ++ envName ++ "}")
  return []

skipMultilineDetection :: Parser String st String
skipMultilineDetection = try (string "\\%")
                         <|>  skipTeXComment
                         <|>  skipTexEnvironment "split"
                         <|>  skipTexEnvironment "aligned"
                         <|>  skipTexEnvironment "alignedat"
                         <|>  skipTexEnvironment "cases"

--
-- Writer functions for Scholarly DisplayMath
--

dispMathToLaTeX :: Attr -> String -> String
dispMathToLaTeX (label, classes, _) mathCode
  | "align" `elem` classes = wrapInLatexEnv "align" mathCode
  | "gather" `elem` classes = wrapInLatexEnv "gather" mathCode
  | "math_def" `elem` classes = mathCode
  | otherwise = case label of
                  "" -> wrapInLatexEnv "equation*" mathCode
                  _  -> wrapInLatexEnv "equation" mathCode

--
-- Utility functions for Figures
--

-- Specifies how Ids for Scholarly Figures map to numeric labels, and how they
-- are updated in the list of identifiers
figureIdToNumLabelHandler :: Attr
                          -> ParserState
                          -> (XRefIdentifiers -> [String])
                          -> (XRefIdentifiers -> [String] -> XRefIdentifiers)
                          -> (Attr -> Attr, ParserState -> ParserState)
figureIdToNumLabelHandler attr state idListGetter idListSetter =
  let xrefIds = stateXRefIdents state
  -- numbering can be forcibly disabled by class ".nonumber"
      needId = not (hasClass "nonumber" attr) && getIdentifier attr /= ""
      myIdentifier = getIdentifier attr
      myNumLabel = if needId
                      then length (filter (/= "") $ idListGetter xrefIds) + 1
                      else 0 -- will never be displayed anyways
      newXrefIds = idListSetter xrefIds (idListGetter xrefIds ++ [myIdentifier])
      stateUpdater s = s{ stateXRefIdents = newXrefIds }
      attrUpdater = insertReplaceKeyValIf needId ("numLabel", show myNumLabel)
  in (attrUpdater, stateUpdater)

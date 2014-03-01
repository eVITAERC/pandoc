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
                              processSingleEqn,
                              processMultiEqn,
                              AttributedMath,
                              insertAttrClass
                             )
where

import Data.List ( intercalate, isPrefixOf )
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.Parsing hiding (tableWith)
import Control.Arrow

type AttributedMath = (Attr, String)

-- true only if some element of classes start with "math"
classIsMath :: Attr -> Bool
classIsMath (_,classes,_) = any ("math" `isPrefixOf`) classes

insertAttrClass :: String -> Attr -> Attr
insertAttrClass className attr@(ident, classes, keyval)
  | className `elem` classes = (ident, className:classes, keyval)
  | otherwise = attr

getClasses :: Attr -> [String]
getClasses (_, classes, _) = classes

getIdentifier :: Attr -> String
getIdentifier (identifier, _, _) = identifier

---
--- Process functions for single-equation Scholarly DisplayMath
---

-- Currently does the following:
-- 1) automatically wrap in @aligned@ or @split@ envionrment if needed
-- 2) if attribute has no id, append @\nonumber@ to code
processSingleEqn :: AttributedMath -> AttributedMath
processSingleEqn eqn =
  let processors = [ensureNonumber,
                    ensureLabeled,
                    ensureMultilineEnv]
  in foldr ($) eqn processors

processMultiEqn :: [AttributedMath] -> AttributedMath
processMultiEqn eqnList =
  let processors = [ensureNonumber,
                    ensureLabeled,
                    id *** trim]
      processedEqnList = foldr map eqnList processors
  in concatMultiEquations processedEqnList

-- Automatically surround with split env if naked token @'\\'@ detected,
-- or aligned env if both naked token @'\\'@ and @'&'@ detected.
-- Skipped classes: [math_plain]
ensureMultilineEnv :: AttributedMath -> AttributedMath
ensureMultilineEnv eqn@(attr, content)
  | "math_plain" `elem` (getClasses attr) = eqn
  | hasTeXLinebreak content = if hasTeXAlignment content
                                 then (attr, wrapInLatexEnv "aligned" content)
                                 else (attr, wrapInLatexEnv "split" content)
  | otherwise = eqn

-- if attribute has no id, append @\nonumber@ to code
ensureNonumber :: AttributedMath -> AttributedMath
ensureNonumber eqn@(attr, content) =
  case attr of
    ("",_ ,_) -> (attr, "\\nonumber " ++ content)
    _         -> eqn

-- if attribute has id, append @\label{id}@ to code
-- (does not ensure no duplicate labels)
ensureLabeled :: AttributedMath -> AttributedMath
ensureLabeled eqn@(attr, content) =
  case attr of
    ("",_ ,_)     -> eqn
    (label, _, _) -> (attr, "\\label{" ++ label ++ "} " ++ content)

-- scans first equation for alignment characters,
-- assign @align@ or @gather@ accordingly,
-- then concatenate all lines into one multi-equation displayMath,
-- gathering the idents of all equations into one large list
concatMultiEquations :: [AttributedMath] -> AttributedMath
concatMultiEquations eqnList =
  let eqnContents = map snd eqnList
      multiClass = if hasTeXAlignment (head eqnContents)
                      then "align"
                      else "gather"
  in ( ("", ["math",multiClass], [("labelList",show (map (getIdentifier.fst) eqnList))]),
       intercalate "\\\\\n" eqnContents )

wrapInLatexEnv :: String -> String -> String
wrapInLatexEnv envName content = intercalate "\n" $
            ["\\begin{" ++ envName ++ "}", content, "\\end{" ++ envName ++ "}"]

-- Scan for occurance of @'\\'@ in non-commented parts,
-- not within "split" or "aligned" environment
hasTeXLinebreak :: String -> Bool
hasTeXLinebreak content =
  case parse (skipMany (try ignoreLinebreak
                        <|> try (char '\\' >> notFollowedBy (char '\\') >> return [])
                        <|> try (noneOf ['\\'] >> return []))
               >> (string "\\\\" >> return ())) [] content of
       Left _  -> False
       Right _ -> True

-- Scan for occurance of non-escaped @'&'@ in non-commented parts
-- not within "split" or "aligned" environment
hasTeXAlignment :: String -> Bool
hasTeXAlignment content =
  case parse (skipMany (try ignoreLinebreak
                        <|>  try (string "\\&")
                        <|>  try (noneOf ['&'] >> return []))
              >> (char '&' >> return ())) [] content of
       Left _  -> False
       Right _ -> True

skipTeXComment :: Parser [Char] st [Char]
skipTeXComment = try $ do
  char '%'
  manyTill anyChar $ try $ newline <|> (eof >> return '\n')
  return []

skipTexEnvironment :: String -> Parser [Char] st [Char]
skipTexEnvironment envName = try $ do
  string ("\\begin{" ++ envName ++ "}")
  manyTill anyChar $ try $ string ("\\end{" ++ envName ++ "}")
  return []

ignoreLinebreak :: Parser [Char] st [Char]
ignoreLinebreak = try (string "\\%")
                  <|>  skipTeXComment
                  <|>  skipTexEnvironment "split"
                  <|>  skipTexEnvironment "aligned"
                  <|>  skipTexEnvironment "alignedat"
                  <|>  skipTexEnvironment "cases"

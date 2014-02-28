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
                              AttributedMath
                             )
where

import Data.List ( intercalate, isPrefixOf )
import Text.Pandoc.Definition
import Text.Pandoc.Parsing hiding (tableWith)

type AttributedMath = (Attr, String)

-- true only if some element of classes start with "math"
classIsMath :: Attr -> Bool
classIsMath (_,classes,_) = any ("math" `isPrefixOf`) classes

---
--- Process functions for single-equation Scholarly DisplayMath
---

-- Currently does the following:
-- 1) automatically wrap in @aligned@ or @split@ envionrment if needed
-- 2) if attribute has no id, append @\nonumber@ to code
processSingleEqn :: AttributedMath -> AttributedMath
processSingleEqn (attr@(_,classes,_), content) =
  let ensureMultilineEnv' = if any ("math_plain" ==) classes
                               then id
                               else ensureMultilineEnv
      processors = [ensureNonumber attr, ensureMultilineEnv']
  in (attr, foldr ($) content processors)

-- Automatically surround with split env if naked token @'\\'@ detected,
-- or aligned env if both naked token @'\\'@ and @'&'@ detected
ensureMultilineEnv :: String -> String
ensureMultilineEnv content
  | hasTeXLinebreak content = if hasTeXAlignment content
                                 then wrapInLatexEnv "aligned" content
                                 else wrapInLatexEnv "split" content
  | otherwise = content

-- if attribute has no id, append @\nonumber@ to code
ensureNonumber :: Attr -> String -> String
ensureNonumber attr content =
  case attr of
    ("",_ ,_) -> "\\nonumber " ++ content
    _         -> content

wrapInLatexEnv :: String -> String -> String
wrapInLatexEnv envName content = intercalate "\n" $
            ["\\begin{" ++ envName ++ "}", content, "\\end{" ++ envName ++ "}"]

-- Scan for occurance of @'\\'@ in non-commented parts,
-- not within "split" or "aligned" environment
hasTeXLinebreak :: String -> Bool
hasTeXLinebreak content =
  case parse (skipMany (try (string "\\%")
                        <|> skipTeXComment
                        <|> skipTexEnvironment "split"
                        <|> skipTexEnvironment "aligned"
                        <|> skipTexEnvironment "alignedat"
                        <|> skipTexEnvironment "cases"
                        <|> try (char '\\' >> notFollowedBy (char '\\') >> return [])
                        <|> try (noneOf ['\\'] >> return []))
               >> (string "\\\\" >> return ())) [] content of
       Left _  -> False
       Right _ -> True

-- Scan for occurance of non-escaped @'&'@ in non-commented parts
-- not within "split" or "aligned" environment
hasTeXAlignment :: String -> Bool
hasTeXAlignment content =
  case parse (skipMany (try (string "\\%")
                        <|>  skipTeXComment
                        <|>  skipTexEnvironment "split"
                        <|>  skipTexEnvironment "aligned"
                        <|>  skipTexEnvironment "alignedat"
                        <|>  skipTexEnvironment "cases"
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
  manyTill anyChar $ string ("\\end{" ++ envName ++ "}")
  return []

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
module Text.Pandoc.Scholarly (classIsMath)
where

import Data.List ( transpose, sortBy, findIndex, intersperse, intercalate,
                   isPrefixOf, isInfixOf )
import qualified Data.Map as M
import Data.Ord ( comparing )
import Data.Char ( isAlphaNum, toLower )
import Data.Maybe
import Text.Pandoc.Definition
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.HashMap.Strict as H
import qualified Text.Pandoc.Builder as B
import qualified Text.Pandoc.UTF8 as UTF8
import qualified Data.Vector as V
import Text.Pandoc.Builder (Inlines, Blocks, trimInlines, (<>))
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Text.Pandoc.XML (fromEntities)
import Text.Pandoc.Parsing hiding (tableWith)
import Text.Pandoc.Readers.LaTeX ( rawLaTeXInline, rawLaTeXBlock, anyControlSeq )
import Text.Pandoc.Readers.HTML ( htmlTag, htmlInBalanced, isInlineTag, isBlockTag,
                                  isTextTag, isCommentTag )
import Data.Monoid (mconcat, mempty)
import Control.Applicative ((<$>), (<*), (*>), (<$))
import Control.Monad
import System.FilePath (takeExtension, addExtension)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match (tagOpen)
import qualified Data.Set as Set

type AnyMath = Math MathType
type InlineM = Math InlineMath
type DispM = Math DisplayMath Attr

-- true only if some element of classes start with "math"
classIsMath :: Attr -> Bool
classIsMath (_,classes,_) = any ("math" `isPrefixOf`) classes

-- filter math raw content
filtMathContent :: AnyMath String -> (String -> String) -> AnyMath String
filtMathContent (InlineM content) filtf = InlineM $ filtf content
filtMathContent (Math DisplayMath attr content) filtf = Math DisplayMath attr $ filtf content

-- filter displayMath attr
filtMathAttr :: DispM String -> (Attr -> Attr) -> DispM String
filtMathAttr (Math DisplayMath attr content) filtf = Math DisplayMath (filtf attr) content

---
--- Process functions for single-equation Scholarly DisplayMath
---

processSingleEqn :: DispM String -> DispM String

-- Automatically surround with split env if token @'\\'@ detected, or aligned env if both token @'\\'@ and @'&'@ detected
addMultilineEnv :: String -> String
addEnvAligned content
  | hasTeXLinebreak content =
                        if hasTeXAlignment
                          then warpInAlign
  | otherwise = content


-- Scan for occurance of @'\\'@ in non-commented parts
hasTeXLinebreak :: String -> Bool
hasTeXLinebreak content =
  case parse (skipMany try $ string "\\%" <|> skipTeXComment <|> anyControlSeq <|> noneOf ['\\'] >> char '&')

-- Scan for occurance of non-escaped @'&'@ in non-commented parts
hasTeXAlignment :: String -> Bool

wrapInTeXEnv :: String -> String -> String
wrapInTeXEnv envName content = intercalate '\n' $
            ["\\begin{" ++ envName ++ "}", content, "\\end{" ++ envName ++ "}"]

skipTeXComment :: Parser [Char] st ()
skipTeXComment = char '%' >> manyTill anyChar $ try (newline <|> eof) >> return ()

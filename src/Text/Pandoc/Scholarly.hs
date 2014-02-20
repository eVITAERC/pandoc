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

Parsing utilitiles for Scholarly Markdown extensions.
-}
module Text.Pandoc.Scholarly ( scholarlyMath,
                             classIsMath
                             )
where

import Data.List ( transpose, sortBy, findIndex, intersperse, intercalate, isPrefixOf )
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
import Text.Pandoc.Readers.LaTeX ( rawLaTeXInline, rawLaTeXBlock )
import Text.Pandoc.Readers.HTML ( htmlTag, htmlInBalanced, isInlineTag, isBlockTag,
                                  isTextTag, isCommentTag )
import Data.Monoid (mconcat, mempty)
import Control.Applicative ((<$>), (<*), (*>), (<$))
import Control.Monad
import System.FilePath (takeExtension, addExtension)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match (tagOpen)
import qualified Data.Set as Set

type ScholarlyParser = Parser [Char] ParserState

--
-- Scholarly Markdown math extensions
--

scholarlyMath :: ScholarlyParser (F Inlines)
scholarlyMath

-- InlineMath delimted by double backticks
inlineMath :: ScholarlyParser (F Inlines)
inlineMath = guardEnabled Ext_scholarly_markdown >>
                           mathInlineWith' (exactly 2 '`') (exactly 2 '`')

-- DisplayMath blocks with attributes inside a fenced code block
-- only match if class of fenced code block starts with math
displayMath :: ScholarlyParser (F Inlines)
displayMath = try $ do
  guardEnabled Ext_scholarly_markdown
  c <- try (guardEnabled Ext_fenced_code_blocks >> lookAhead (char '~'))
     <|> (guardEnabled Ext_backtick_code_blocks >> lookAhead (char '`'))
  size <- blockDelimiter (== c) Nothing
  skipMany spaceChar
  attr <- option ([],[],[]) $
            try (guardEnabled Ext_fenced_code_attributes >> attributes)
           <|> ((\x -> ("",[x],[])) <$> identifier)
  guard $ classIsMath attr
  blankline
  contents <- manyTill anyLine (blockDelimiter (== c) (Just size))
  return $ return $ B.displayMathWith attr $ intercalate "\n" contents

-- TODO: multilineMath :: ScholarlyParser (F Inlines)

-- TODO: mathDefinitions :: ScholarlyParser (F Inlines)

-- true only if some element of classes start with "math"
classIsMath :: Attr -> Bool
classIsMath (_,classes,_) = any ("math" `isPrefixOf`) classes

--
-- Scholarly Markdown figures
--

--
-- Scholarly Markdown numerical cross-references
--

--
-- Scholarly Markdown statements
--

--
-- Scholarly Markdown algorithm/listings
--

--
-- Scholarly Markdown tables
--



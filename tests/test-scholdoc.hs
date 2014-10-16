{-# OPTIONS_GHC -Wall #-}

module Main where

import Test.Framework
import GHC.IO.Encoding
import qualified Tests.Old
import qualified Tests.Readers.LaTeX
import qualified Tests.Readers.Markdown
import qualified Tests.Writers.LaTeX
import qualified Tests.Writers.HTML
import qualified Tests.Writers.Native
import qualified Tests.Writers.Markdown
import qualified Tests.Writers.Plain
import qualified Tests.Shared
import qualified Tests.Walk
import Text.Pandoc.Shared (inDirectory)
import System.Environment (getArgs)

tests :: [Test]
tests = [ testGroup "Old" Tests.Old.tests
        , testGroup "Shared" Tests.Shared.tests
        , testGroup "Walk" Tests.Walk.tests
        , testGroup "Writers"
          [ testGroup "Native" Tests.Writers.Native.tests
          , testGroup "LaTeX" Tests.Writers.LaTeX.tests
          , testGroup "HTML" Tests.Writers.HTML.tests
          , testGroup "Markdown" Tests.Writers.Markdown.tests
          , testGroup "Plain" Tests.Writers.Plain.tests
          ]
        , testGroup "Readers"
          [ testGroup "LaTeX" Tests.Readers.LaTeX.tests
          , testGroup "Markdown" Tests.Readers.Markdown.tests
          ]
        ]

main :: IO ()
main = do
  setLocaleEncoding utf8
  args <- getArgs
  inDirectory "tests" $ defaultMainWithArgs tests args

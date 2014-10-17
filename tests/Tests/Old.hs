module Tests.Old (tests) where

import Test.Framework (testGroup, Test )
import Test.Framework.Providers.HUnit
import Test.HUnit ( assertBool )
import System.Environment.Executable (getExecutablePath)
import System.IO ( openTempFile, stderr )
import System.Process ( runProcess, waitForProcess )
import System.FilePath ( (</>), (<.>), takeDirectory, splitDirectories, joinPath )
import System.Directory
import System.Exit
import Data.Algorithm.Diff
import Text.Pandoc.Shared ( normalize )
import Text.Pandoc.Options
import Text.Pandoc.Writers.Native ( writeNative )
import Text.Pandoc.Readers.Native ( readNative )
import Prelude hiding ( readFile )
import qualified Data.ByteString.Lazy as B
import Text.Pandoc.UTF8 (toStringLazy)
import Text.Printf

readFileUTF8 :: FilePath -> IO String
readFileUTF8 f = B.readFile f >>= return . toStringLazy

data TestResult = TestPassed
                | TestError ExitCode
                | TestFailed String FilePath [Diff String]
     deriving (Eq)

instance Show TestResult where
  show TestPassed     = "PASSED"
  show (TestError ec) = "ERROR " ++ show ec
  show (TestFailed cmd file d) = '\n' : dash ++
                                 "\n--- " ++ file ++
                                 "\n+++ " ++ cmd ++ "\n" ++ showDiff (1,1) d ++
                                 dash
    where dash = replicate 72 '-'

showDiff :: (Int,Int) -> [Diff String] -> String
showDiff _ []             = ""
showDiff (l,r) (First ln : ds) =
  printf "+%4d " l ++ ln ++ "\n" ++ showDiff (l+1,r) ds
showDiff (l,r) (Second ln : ds) =
  printf "-%4d " r ++ ln ++ "\n" ++ showDiff (l,r+1) ds
showDiff (l,r) (Both _ _ : ds) =
  showDiff (l+1,r+1) ds

tests :: [Test]
tests = [ testGroup "markdown"
          [ testGroup "writer"
            $ writerTests "markdown" ++ lhsWriterTests "markdown"
          , testGroup "reader"
            [ test "basic" ["-r", "markdown", "-w", "native", "-s", "-S"]
              "testsuite.txt" "testsuite.native"
            , test "tables" ["-r", "markdown", "-w", "native", "--columns=80"]
              "tables.txt" "tables.native"
            , test "pipe tables" ["-r", "markdown", "-w", "native", "--columns=80"]
              "pipe-tables.txt" "pipe-tables.native"
            , test "more" ["-r", "markdown", "-w", "native", "-S"]
              "markdown-reader-more.txt" "markdown-reader-more.native"
            , test "scholdoc" ["-r", "markdown_scholarly", "-w", "native", "-s", "-S", "-R"]
              "scholdoc.txt" "scholdoc.native"
            , lhsReaderTest "markdown+lhs"
            ]
          , testGroup "citations"
            [ test "citations" ["-r", "markdown", "-w", "native"]
              "markdown-citations.txt" "markdown-citations.native"
            ]
          ]
        , testGroup "latex"
          [ testGroup "writer" (writerTests "latex" ++ lhsWriterTests "latex" ++
            [ test "scholdoc"  ["-r", "markdown_scholarly", "-w", "latex", "-S", "-R",
                                "--chapters", "--columns=72"]
              "scholdoc.txt" "scholdoc.latex"
            ])
          ]
        , testGroup "html"
          [ testGroup "writer" (writerTests "html" ++ lhsWriterTests "html" ++
            [ test "scholdoc"  ["-r", "markdown_scholarly", "-w", "html5", "-S", "-R",
                                "--template=scholmdTemplate_bodyOnly.html5",
                                "--mathjax", "--no-mathjax-cdn", "--columns=72"]
              "scholdoc.txt" "scholdoc.html"
            ])
          ]
        , testGroup "s5"
          [ s5WriterTest "basic" ["-s"] "s5"
          , s5WriterTest "fancy" ["-s","-m","-i"] "s5"
          , s5WriterTest "fragment" [] "html"
          , s5WriterTest "inserts"  ["-s", "-H", "insert",
            "-B", "insert", "-A", "insert", "-c", "main.css"] "html"
          ]
        , testGroup "native"
          [ testGroup "writer" $ writerTests "native"
          , test "reader" ["-r", "native", "-w", "native", "-s"]
            "testsuite.native" "testsuite.native"
          ]
        , testGroup "haddock"
          [ testGroup "writer" $ writerTests "haddock"
          ]
        , testGroup "other writers" $ map (\f -> testGroup f $ writerTests f)
          [ "man" , "plain"
          ]
        ]

-- makes sure file is fully closed after reading
readFile' :: FilePath -> IO String
readFile' f = do s <- readFileUTF8 f
                 return $! (length s `seq` s)

lhsWriterTests :: String -> [Test]
lhsWriterTests format
  = [ t "lhs to normal" format
    , t "lhs to lhs"    (format ++ "+lhs")
    ]
  where
    t n f = test n ["--columns=78", "-r", "native", "-s", "-w", f]
             "lhs-test.native" ("lhs-test" <.> f)

lhsReaderTest :: String -> Test
lhsReaderTest format =
  testWithNormalize normalizer "lhs" ["-r", format, "-w", "native"]
    ("lhs-test" <.> format) norm
   where normalizer = writeNative def . normalize . readNative
         norm = if format == "markdown+lhs"
                   then "lhs-test-markdown.native"
                   else "lhs-test.native"

writerTests :: String -> [Test]
writerTests format
  = [ test "basic"  (opts ++ ["-s"]) "testsuite.native" ("writer" <.> format)
    , test "tables" opts             "tables.native"    ("tables" <.> format)
    ]
  where
    opts = ["-r", "native", "-w", format, "--columns=78"]

s5WriterTest :: String -> [String] -> String -> Test
s5WriterTest modifier opts format
  = test (format ++ " writer (" ++ modifier ++ ")")
    (["-r", "native", "-w", format] ++ opts)
    "s5.native"  ("s5-" ++ modifier <.> "html")

-- | Run a test without normalize function, return True if test passed.
test :: String    -- ^ Title of test
     -> [String]  -- ^ Options to pass to scholdoc
     -> String    -- ^ Input filepath
     -> FilePath  -- ^ Norm (for test results) filepath
     -> Test
test = testWithNormalize id

-- | Run a test with normalize function, return True if test passed.
testWithNormalize  :: (String -> String) -- ^ Normalize function for output
                   -> String    -- ^ Title of test
                   -> [String]  -- ^ Options to pass to scholdoc
                   -> String    -- ^ Input filepath
                   -> FilePath  -- ^ Norm (for test results) filepath
                   -> Test
testWithNormalize normalizer testname opts inp norm = testCase testname $ do
  -- find scholdoc executable relative to test-scholdoc
  -- First, try in same directory (e.g. if both in ~/.cabal/bin)
  -- Second, try ../scholdoc (e.g. if in dist/XXX/build/test-scholdoc)
  scholdocPath <- do
    testExePath <- getExecutablePath
    let testExeDir = takeDirectory testExePath
    found <- doesFileExist (testExeDir </> "scholdoc")
    return $ if found
                then testExeDir </> "scholdoc"
                else case splitDirectories testExeDir of
                           [] -> error "test-scholdoc: empty testExeDir"
                           xs -> joinPath (init xs) </> "scholdoc" </> "scholdoc"
  (outputPath, hOut) <- openTempFile "" "scholdoc-test"
  let inpPath = inp
  let normPath = norm
  let options = ["--emulate-pandoc", "--data-dir", ".." </> "data"] ++ [inpPath] ++ opts
  let cmd = scholdocPath ++ " " ++ unwords options
  ph <- runProcess scholdocPath options Nothing
        (Just [("TMP","."),("LANG","en_US.UTF-8"),("HOME", "./")]) Nothing (Just hOut)
        (Just stderr)
  ec <- waitForProcess ph
  result  <- if ec == ExitSuccess
                then do
                  -- filter \r so the tests will work on Windows machines
                  outputContents <- readFile' outputPath >>=
                    return . filter (/='\r') . normalizer
                  normContents <- readFile' normPath >>=
                    return . filter (/='\r') . normalizer
                  if outputContents == normContents
                     then return TestPassed
                     else return
                          $ TestFailed cmd normPath
                          $ getDiff (lines outputContents) (lines normContents)
                else return $ TestError ec
  removeFile outputPath
  assertBool (show result) (result == TestPassed)

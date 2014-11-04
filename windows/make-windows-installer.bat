@echo off
cd ..
cabal update
cabal sandbox init
cabal clean
cabal install hsb2hs
if %errorlevel% neq 0 exit /b %errorlevel%
cabal install -v1 --force --reinstall --flags="embed_data_files" . scholdoc-citeproc
if %errorlevel% neq 0 exit /b %errorlevel%
strip .\.cabal-sandbox\bin\scholdoc.exe
strip .\.cabal-sandbox\bin\scholdoc-citeproc.exe
copy COPYRIGHT COPYRIGHT.txt
:: for /f "tokens=1-2 delims= " %%a in ('.\.cabal-sandbox\bin\scholdoc --version') do (
::   @set VERSION=%%b
::   goto :next
::   )
@set VERSION=0.1.3
:next
if "%VERSION%" == "" (
  echo Error: could not determine version number.
  exit /b 1 
)
echo Detected version %VERSION%
cd windows
echo Creating msi...
candle -dVERSION=%VERSION% scholdoc.wxs
if %errorlevel% neq 0 exit /b %errorlevel%
light  -sw1076 -ext WixUIExtension -out scholdoc-%VERSION%-alpha-windows.msi scholdoc.wixobj
if %errorlevel% neq 0 exit /b %errorlevel%
:: echo Starting kSign: sign, then quit kSign to complete the build...
:: kSign

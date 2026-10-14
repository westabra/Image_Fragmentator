@echo off
setlocal
set "suite_root=%~dp0.."
set "r_exe="
if exist "%suite_root%\R_PATH.txt" set /p "r_exe="<"%suite_root%\R_PATH.txt"
if defined r_exe goto found
for /f "delims=" %%R in ('where Rscript.exe 2^>nul') do if not defined r_exe set "r_exe=%%R"
if defined r_exe goto found
for /d %%D in ("%ProgramFiles%\R\R-*") do if exist "%%D\bin\Rscript.exe" set "r_exe=%%D\bin\Rscript.exe"
if defined r_exe goto found
for /d %%D in ("%LOCALAPPDATA%\Programs\R\R-*") do if exist "%%D\bin\Rscript.exe" set "r_exe=%%D\bin\Rscript.exe"
:found
if not defined r_exe goto missing
if not exist "%r_exe%" goto missing
"%r_exe%" --vanilla "%suite_root%\run_fragmentator.R" %1
set "result=%errorlevel%"
if not "%result%"=="0" (pause) else if /I "%1"=="setup" pause
exit /b %result%
:missing
echo Rscript.exe was not found. Install R 4.5 x64 or create R_PATH.txt with its full path.
pause
exit /b 1

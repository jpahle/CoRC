call C:\BuildTools\Common7\Tools\VsDevCmd.bat -arch=amd64

cd copasi-dependencies\
echo "===== Deleting old dependencies build"
rmdir /s /q tmp
rmdir /s /q bin
echo "===== Building dependencies"
setlocal
SET BUILD_DIR=C:\copasi-dependencies\tmp
call .\createWindows.bat
rmdir C:\copasi-dependencies
endlocal

echo "===== Looking for R"
for /f %%i in ('dir /b "%ProgramFiles%\R"') do set RVERSION=%%i
echo "===== R at %ProgramFiles%\R\%RVERSION%"

echo "===== Copying CopasiVersion.h"
copy /Y CopasiVersion.h COPASI\copasi\

mkdir C:\corc_windows_x64\
cd C:\corc_windows_x64\
echo "===== Running CMake"
cmake ^
  -G Ninja ^
  -DCMAKE_BUILD_TYPE=Release ^
  -DCMAKE_C_COMPILER=cl ^
  -DCMAKE_CXX_COMPILER=cl ^
  -DBUILD_GUI=OFF ^
  -DBUILD_SE=OFF ^
  -DENABLE_R=ON ^
  -DCOPASI_DEPENDENCY_DIR="C:/work/copasi-dependencies/bin/" ^
  -DSWIG_EXECUTABLE="C:/swigwin/swig.exe" ^
  -DR_INCLUDE_DIRS="%ProgramFiles%/R/%RVERSION%/include/" ^
  -DR_INTERPRETER="%ProgramFiles%/R/%RVERSION%/bin/R.exe" ^
  -DR_LIB="C:/Rlib/Rlib.lib" ^
  C:/work/COPASI/
echo "===== Running Ninja"
ninja binding_r_lib

cd C:\work\
echo "===== Export the result"
rmdir corc_windows_x64\
robocopy C:\corc_windows_x64\ corc_windows_x64\ /e /nfl /move
echo "===== Copying results into libs/"
mkdir libs\
copy /Y corc_windows_x64\copasi\bindings\R\COPASI.dll libs\COPASI_windows_x86_64.dll
copy /Y corc_windows_x64\copasi\bindings\R\COPASI.dll libs\COPASI.dll
copy /Y corc_windows_x64\copasi\bindings\R\COPASI.R libs\swig_wrapper.R

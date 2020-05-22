call C:\BuildTools\Common7\Tools\VsDevCmd.bat -arch=amd64

echo "===== Looking for R"
for /f %%i in ('dir /b "%ProgramFiles%\R"') do set RVERSION=%%i
echo "===== R at %ProgramFiles%\R\%RVERSION%"

echo "===== Copying CopasiVersion.h"
copy /Y CopasiVersion.h COPASI\copasi\

echo "===== Copying the current build"
robocopy corc_windows_x64\ C:\corc_windows_x64\ /e /nfl /ndl
mkdir C:\corc_windows_x64\
cd C:\corc_windows_x64\
echo "===== Deleting copasi_wrapper.cpp and COPASI.R"
del copasi\bindings\R\copasi_wrapper.cpp
del copasi\bindings\R\COPASI.R
echo "===== Running CMake"
cmake ^
  -G Ninja ^
  -DCMAKE_BUILD_TYPE=Release ^
  -DCMAKE_C_COMPILER=cl ^
  -DCMAKE_CXX_COMPILER=cl ^
  -DWITH_STATIC_RUNTIME=ON ^
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
rmdir corc_windows_x64\ /s /q
robocopy C:\corc_windows_x64\ corc_windows_x64\ /e /nfl /ndl /move
echo "===== Copying results into libs/"
mkdir libs\
copy /Y corc_windows_x64\copasi\bindings\R\COPASI.dll libs\COPASI_windows_x86_64.dll
copy /Y corc_windows_x64\copasi\bindings\R\COPASI.dll libs\COPASI.dll
copy /Y corc_windows_x64\copasi\bindings\R\COPASI.R libs\swig_wrapper.R

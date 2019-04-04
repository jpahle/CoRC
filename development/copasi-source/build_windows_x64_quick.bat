echo "===== Looking for R"
for /f %%i in ('dir /b "%ProgramFiles%\R"') do set RVERSION=%%i
echo "===== R at %ProgramFiles%\R\%RVERSION%"

echo "===== Copying CopasiVersion.h"
copy /Y CopasiVersion.h COPASI\copasi\

mkdir corc_windows_x64\
cd corc_windows_x64\
echo "===== Deleting copasi_wrapper.cpp and COPASI.R"
del copasi\bindings\R\copasi_wrapper.cpp
del copasi\bindings\R\COPASI.R
echo "===== Running CMake"
cmake ^
  -G Ninja ^
  -DCMAKE_BUILD_TYPE=Release ^
  -DCMAKE_C_COMPILER=cl ^
  -DCMAKE_CXX_COMPILER=cl ^
  -DBUILD_GUI=OFF ^
  -DBUILD_SE=OFF ^
  -DENABLE_R=ON ^
  -DCOPASI_DEPENDENCY_DIR="../copasi-dependencies/bin/" ^
  -DSWIG_EXECUTABLE="C:/swigwin/swig.exe" ^
  -DR_INCLUDE_DIRS="%ProgramFiles%/R/%RVERSION%/include/" ^
  -DR_INTERPRETER="%ProgramFiles%/R/%RVERSION%/bin/R.exe" ^
  -DR_LIB="C:/Rlib/Rlib.lib" ^
  ../COPASI/
echo "===== Running Ninja"
ninja binding_r_lib
cd ..\

echo "===== Copying results into libs/"
mkdir libs\
copy /Y corc_windows_x64\copasi\bindings\R\COPASI.dll libs\COPASI_windows_x86_64.dll
copy /Y corc_windows_x64\copasi\bindings\R\COPASI.dll libs\COPASI.dll
copy /Y corc_windows_x64\copasi\bindings\R\COPASI.R libs\swig_wrapper.R

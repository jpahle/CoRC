docker build --pull -t corc_compiler_windows_x64 compilers/windows_x64

docker run --rm -v ${PWD}:C:/work corc_compiler_windows_x64 build_windows_x64_quick.bat

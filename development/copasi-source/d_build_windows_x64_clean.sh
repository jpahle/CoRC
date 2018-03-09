#!/bin/bash
set -e
set -x

eval $(docker-machine env 2016-box)

docker build -t corc_compiler_windows_x64 compilers/windows_x64

docker run --rm -v C:$(pwd):C:/work corc_compiler_windows_x64 build_windows_x64_clean.bat

#!/bin/bash
set -e
set -x

: ===== Adding homebrew QT to PATH
export PATH="/usr/local/opt/qt/bin:$PATH"

: ===== Setting ENV
CMAKE=${CMAKE:=cmake}

cd copasi-dependencies/
: ===== Deleting old dependencies build
[[ -d "tmp_darwin_x64" ]] && rm -r tmp_darwin_x64/
[[ -d "bin_darwin_x64" ]] && rm -r bin_darwin_x64/
mkdir tmp_darwin_x64/
cd tmp_darwin_x64/
: ===== Building dependencies
cmake \
	-DGIT_SUBMODULE=OFF \
	-DCMAKE_INSTALL_PREFIX=../bin_darwin_x64 \
	-DBUILD_UI_DEPS=OFF \
	../
make
cd ../../

: ===== Copying CopasiVersion.h
cp CopasiVersion.h COPASI/copasi/

: ===== Deleting old build
[[ -d "corc_darwin_x64" ]] && rm -r corc_darwin_x64/
mkdir corc_darwin_x64/
cd corc_darwin_x64/
: ===== Running CMake
cmake \
	-DCMAKE_BUILD_TYPE=Release \
	-DBUILD_GUI=OFF \
	-DBUILD_SE=OFF \
	-DENABLE_JIT=ON \
	-DENABLE_R=ON \
	-DR_USE_DYNAMIC_LOOKUP=ON \
	-DCOPASI_DEPENDENCY_DIR=../copasi-dependencies/bin_darwin_x64/ \
	../COPASI/
: ===== Running Make
make binding_r_lib
cd ../

: ===== Copying results into libs/
mkdir -p libs/
cp corc_darwin_x64/copasi/bindings/R/COPASI.so libs/COPASI_darwin_x86_64.so
cp corc_darwin_x64/copasi/bindings/R/COPASI.so libs/COPASI.so
cp corc_darwin_x64/copasi/bindings/R/COPASI.R libs/swig_wrapper.R

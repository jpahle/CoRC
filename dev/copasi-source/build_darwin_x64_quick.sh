#!/bin/bash
set -e
set -x

: ===== Copying CopasiVersion.h
cp CopasiVersion.h COPASI/copasi/

mkdir -p corc_darwin_x64/
cd corc_darwin_x64/
: ===== Deleting copasi_wrapper.cpp and COPASI.R
rm -f copasi/bindings/R/copasi_wrapper.cpp copasi/bindings/R/COPASI.R
: ===== Running CMake
cmake \
	-DCMAKE_BUILD_TYPE=Release \
	-DBUILD_GUI=OFF \
	-DBUILD_SE=OFF \
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

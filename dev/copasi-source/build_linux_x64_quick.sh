#!/bin/bash
set -e
set -x

: ===== Copying CopasiVersion.h
cp CopasiVersion.h COPASI/copasi/

mkdir -p corc_linux_x64/
cd corc_linux_x64/
: ===== Deleting copasi_wrapper.cpp and COPASI.R
rm -f copasi/bindings/R/copasi_wrapper.cpp copasi/bindings/R/COPASI.R
: ===== Running CMake
cmake3 \
	-DCMAKE_BUILD_TYPE=Release \
	-DBUILD_GUI=OFF \
	-DBUILD_SE=OFF \
	-DENABLE_R=ON \
	-DR_USE_DYNAMIC_LOOKUP=ON \
	-DCOPASI_DEPENDENCY_DIR=../copasi-dependencies/bin_linux_x64/ \
	-DR_INCLUDE_DIRS=/usr/lib64/R/include/ \
	-DR_LIB=/usr/lib64/R/lib/libR.so \
	-DR_INTERPRETER=/usr/bin/R \
	../COPASI/
: ===== Running Make
make binding_r_lib
cd ../

: ===== Copying results into libs/
mkdir -p libs/
cp corc_linux_x64/copasi/bindings/R/COPASI.so libs/COPASI_unix_x86_64.so
cp corc_linux_x64/copasi/bindings/R/COPASI.so libs/COPASI.so
cp corc_linux_x64/copasi/bindings/R/COPASI.R libs/swig_wrapper.R

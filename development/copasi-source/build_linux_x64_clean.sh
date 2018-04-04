#!/bin/bash
set -e
set -x

cd copasi-dependencies/
: ===== Deleting old dependencies build
rm -rf tmp_linux_x64/ bin_linux_x64/
: ===== Building dependencies
BUILD_DIR=${PWD}/tmp_linux_x64 \
	INSTALL_DIR=${PWD}/bin_linux_x64 \
	./createLinux.sh
cd ../

: ===== Copying CopasiVersion.h
cp CopasiVersion.h COPASI/copasi/

: ===== Deleting old build
rm -rf corc_linux_x64/
mkdir corc_linux_x64/
cd corc_linux_x64/
: ===== Running CMake
cmake \
	-DCMAKE_BUILD_TYPE=Release \
	-DBUILD_GUI=OFF \
	-DBUILD_SE=OFF \
	-DENABLE_R=ON \
	-DR_USE_DYNAMIC_LOOKUP=ON \
	-DCOPASI_DEPENDENCY_DIR=../copasi-dependencies/bin_linux_x64/ \
	-DR_INCLUDE_DIRS=/usr/local/lib64/R/include/ \
	-DR_LIB=/usr/local/lib64/R/lib/libR.so \
	-DR_INTERPRETER=/usr/local/bin/R \
	../COPASI/
: ===== Running Make
make binding_r_lib
cd ../

: ===== Copying results into libs/
mkdir -p libs/
cp corc_linux_x64/copasi/bindings/R/COPASI.so libs/COPASI_unix_x86_64.so
cp corc_linux_x64/copasi/bindings/R/COPASI.so libs/COPASI.so
cp corc_linux_x64/copasi/bindings/R/COPASI.R libs/swig_wrapper.R

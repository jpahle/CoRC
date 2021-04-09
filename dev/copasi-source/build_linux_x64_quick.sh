#!/bin/bash
set -e
set -x

LINUX_TAG=${LINUX_TAG:=default_linux_x64}
CMAKE=${CMAKE:=cmake}

: ===== Copying CopasiVersion.h
cp CopasiVersion.h COPASI/copasi/

mkdir -p corc_${LINUX_TAG}/
cd corc_${LINUX_TAG}/
: ===== Deleting copasi_wrapper.cpp and COPASI.R
rm -f copasi/bindings/R/copasi_wrapper.cpp copasi/bindings/R/COPASI.R
: ===== Running CMake
${CMAKE} \
	-DCMAKE_BUILD_TYPE=Release \
	-DF2C_INTEGER=int \
	-DF2C_LOGICAL=int \
	-DBUILD_GUI=OFF \
	-DBUILD_SE=OFF \
	-DENABLE_JIT=OFF \
	-DENABLE_R=ON \
	-DR_USE_DYNAMIC_LOOKUP=ON \
	-DCOPASI_DEPENDENCY_DIR=../copasi-dependencies/bin_${LINUX_TAG}/ \
	-DR_INCLUDE_DIRS=/usr/lib64/R/include/ \
	-DR_LIB=/usr/lib64/R/lib/libR.so \
	-DR_INTERPRETER=/usr/bin/R \
	../COPASI/
: ===== Running Make
make -j$(nproc) binding_r_lib
cd ../

: ===== Copying results into libs/
mkdir -p libs/
cp corc_${LINUX_TAG}/copasi/bindings/R/COPASI.so libs/COPASI_${LINUX_TAG}.so
cp corc_${LINUX_TAG}/copasi/bindings/R/COPASI.so libs/COPASI.so
cp corc_${LINUX_TAG}/copasi/bindings/R/COPASI.R libs/swig_wrapper.R

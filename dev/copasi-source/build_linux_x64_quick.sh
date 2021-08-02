#!/bin/bash
set -e
set -x

LINUX_TAG=${LINUX_TAG:=default_linux_x64}
CMAKE=${CMAKE:=cmake}
R_INCLUDE_DIRS=${R_INCLUDE_DIRS:=/usr/lib64/R/include/}
R_LIB=${R_LIB:=/usr/lib64/R/lib/libR.so}
R_INTERPRETER=${R_INTERPRETER:=/usr/bin/R}

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
	-DR_INCLUDE_DIRS=$R_INCLUDE_DIRS \
	-DR_LIB=$R_LIB \
	-DR_INTERPRETER=$R_INTERPRETER \
	../COPASI/
: ===== Running Make
make -j$(nproc) binding_r_lib
cd ../

: ===== Copying results into libs/
mkdir -p libs/
cp corc_${LINUX_TAG}/copasi/bindings/R/COPASI.so libs/COPASI_${LINUX_TAG}.so
cp corc_${LINUX_TAG}/copasi/bindings/R/COPASI.so libs/COPASI.so
cp corc_${LINUX_TAG}/copasi/bindings/R/COPASI.R libs/swig_wrapper.R

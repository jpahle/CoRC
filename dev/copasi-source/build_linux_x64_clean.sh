#!/bin/bash
set -e
set -x

: ===== Setting ENV
LINUX_TAG=${LINUX_TAG:=default_linux_x64}
CMAKE=${CMAKE:=cmake}
R_INCLUDE_DIRS=${R_INCLUDE_DIRS:=/usr/lib64/R/include/}
R_LIB=${R_LIB:=/usr/lib64/R/lib/libR.so}
R_INTERPRETER=${R_INTERPRETER:=/usr/bin/R}

cd copasi-dependencies/
: ===== Deleting old dependencies build
[[ -d "tmp_${LINUX_TAG}" ]] && rm -r tmp_${LINUX_TAG}/
[[ -d "bin_${LINUX_TAG}" ]] && rm -r bin_${LINUX_TAG}/
mkdir tmp_${LINUX_TAG}/
cd tmp_${LINUX_TAG}/
: ===== Building dependencies
${CMAKE} \
	-DGIT_SUBMODULE=OFF \
	-DCMAKE_INSTALL_PREFIX=../bin_${LINUX_TAG} \
	-DBUILD_UI_DEPS=FALSE \
	-DCOMMON_CMAKE_OPTIONS="-DF2C_INTEGER=int;-DF2C_LOGICAL=int" \
	../
make -j$(nproc)
cd ../../

: ===== Copying CopasiVersion.h
cp CopasiVersion.h COPASI/copasi/

: ===== Deleting old build
[[ -d "corc_${LINUX_TAG}" ]] && rm -r corc_${LINUX_TAG}/
mkdir corc_${LINUX_TAG}/
cd corc_${LINUX_TAG}/
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

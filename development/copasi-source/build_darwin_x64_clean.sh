#!/bin/bash
set -e
set -x

BUILD_NAME=darwin
DEP_BUILD_DIR=tmp_${BUILD_NAME}_x64
DEP_INSTALL_DIR=bin_${BUILD_NAME}_x64
CORC_BUILD_DIR=corc_${BUILD_NAME}_x64
R_LOC=/opt/R-resources

cd copasi-dependencies/
rm -rf ${DEP_BUILD_DIR}/ ${DEP_INSTALL_DIR}/
BUILD_DIR=${PWD}/${DEP_BUILD_DIR} \
	INSTALL_DIR=${PWD}/${DEP_INSTALL_DIR} \
	./createOSX-qt5-cross.sh
cd ../

cp CopasiVersion.h COPASI/copasi/

rm -rf ${CORC_BUILD_DIR}/
mkdir ${CORC_BUILD_DIR}/
cd ${CORC_BUILD_DIR}/
cmake \
	-DCMAKE_TOOLCHAIN_FILE=/opt/toolchain-apple-darwin.cmake \
	-DCMAKE_BUILD_TYPE=Release \
	-DBUILD_GUI=OFF \
	-DBUILD_SE=OFF \
	-DENABLE_R=ON \
	-DR_INCLUDE_DIRS=${R_LOC}/include/ \
	-DR_LIB=${R_LOC}/lib/libR.dylib \
	-DR_USE_DYNAMIC_LOOKUP=ON \
	-DCOPASI_DEPENDENCY_DIR=../copasi-dependencies/${DEP_INSTALL_DIR}/ \
	../COPASI/
make binding_r_lib
cd ../

mkdir -p libs/
cp ${BUILD_DIR}/copasi/bindings/R/COPASI.so libs/COPASI_${BUILD_NAME}_x86_64.so
cp ${BUILD_DIR}/copasi/bindings/R/COPASI.so libs/COPASI.so
cp ${BUILD_DIR}/copasi/bindings/R/COPASI.R libs/swig_wrapper.R

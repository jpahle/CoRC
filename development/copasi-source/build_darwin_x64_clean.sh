#!/bin/bash
set -e
set -x

BUILD_NAME=darwin
BUILD_DIR=corc_${BUILD_NAME}_x64
R_LOC=/opt/R-resources

cd copasi-dependencies/
rm -rf bin/ tmp/ bin_${BUILD_NAME}_x64/
./createOSX-qt5-cross.sh
rm -rf tmp/
mv bin/ bin_${BUILD_NAME}_x64/
cd ../

cp CopasiVersion.h COPASI/copasi/

rm -rf ${BUILD_DIR}/
mkdir ${BUILD_DIR}/
cd ${BUILD_DIR}/
cmake \
	-DCMAKE_TOOLCHAIN_FILE=/opt/toolchain-apple-darwin.cmake \
	-DCMAKE_BUILD_TYPE=Release \
	-DBUILD_GUI=OFF \
	-DBUILD_SE=OFF \
	-DENABLE_R=ON \
	-DR_INCLUDE_DIRS=${R_LOC}/include/ \
	-DR_LIB=${R_LOC}/lib/libR.dylib \
	-DR_USE_DYNAMIC_LOOKUP=ON \
	-DENABLE_NEW_PARSER=OFF \
	-DCOPASI_DEPENDENCY_DIR=../copasi-dependencies/bin_${BUILD_NAME}_x64/ \
	../COPASI/
make
cd ../

mkdir -p libs/
cp ${BUILD_DIR}/copasi/bindings/R/COPASI.so libs/COPASI_${BUILD_NAME}_x86_64.so
cp ${BUILD_DIR}/copasi/bindings/R/COPASI.so libs/COPASI.so
cp ${BUILD_DIR}/copasi/bindings/R/COPASI.R libs/swig_wrapper.R

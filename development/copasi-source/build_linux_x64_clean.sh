#!/bin/bash
set -e
set -x

cd copasi-dependencies/
rm -rf tmp_linux_x64/ bin_linux_x64/
BUILD_DIR=${PWD}/tmp_linux_x64 \
	INSTALL_DIR=${PWD}/bin_linux_x64 \
	./createLinux.sh
cd ../

cp CopasiVersion.h COPASI/copasi/

rm -rf corc_linux_x64/
mkdir corc_linux_x64/
cd corc_linux_x64/
cmake \
	-DCMAKE_BUILD_TYPE=Release \
	-DBUILD_GUI=OFF \
	-DBUILD_SE=OFF \
	-DENABLE_R=ON \
	-DR_USE_DYNAMIC_LOOKUP=ON \
	-DCOPASI_DEPENDENCY_DIR=../copasi-dependencies/bin_linux_x64/ \
	../COPASI/
make binding_r_lib
cd ../

mkdir -p libs/
cp corc_linux_x64/copasi/bindings/R/COPASI.so libs/COPASI_unix_x86_64.so
cp corc_linux_x64/copasi/bindings/R/COPASI.so libs/COPASI.so
cp corc_linux_x64/copasi/bindings/R/COPASI.R libs/swig_wrapper.R

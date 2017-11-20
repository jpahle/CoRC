#!/bin/bash
set -e
set -x

cp CopasiVersion.h COPASI/copasi/

mkdir -p corc_linux_x64/
cd corc_linux_x64/
cmake \
	-DCMAKE_BUILD_TYPE=Release \
	-DBUILD_GUI=OFF \
	-DBUILD_SE=OFF \
	-DENABLE_R=ON \
	-DR_USE_DYNAMIC_LOOKUP=ON \
	-DENABLE_NEW_PARSER=OFF \
	-DCOPASI_DEPENDENCY_DIR=../copasi-dependencies/bin_linux_x64/ \
	../COPASI/
make
cd ../

mkdir -p libs/
cp corc_linux_x64/copasi/bindings/R/COPASI.so libs/COPASI_unix_x86_64.so
cp corc_linux_x64/copasi/bindings/R/COPASI.so libs/COPASI.so
cp corc_linux_x64/copasi/bindings/R/COPASI.R libs/swig_wrapper.R

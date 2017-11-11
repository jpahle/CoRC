#!/bin/bash
# skip dependencies and keep build folder

cd compilers/
docker build -t corc_compiler_linux_x64 ./linux_x64
docker run --rm corc_compiler_linux_x64 > corc_compiler_linux_x64
chmod +x corc_compiler_linux_x64
cd ../

cp CopasiVersion.h COPASI/copasi/

mkdir -p corc_linux_x64/
compilers/corc_compiler_linux_x64 \
	bash -c '
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
	'

mkdir -p libs/
cp corc_linux_x64/copasi/bindings/R/COPASI.so libs/COPASI_unix_x64.so
cp corc_linux_x64/copasi/bindings/R/COPASI.so libs/COPASI.so
cp corc_linux_x64/copasi/bindings/R/COPASI.R libs/swig_wrapper.R

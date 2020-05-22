FROM ubuntu:18.04

RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
		build-essential \
		cmake \
		uuid-dev \
		r-base-core \
		swig \
	&& rm -rf /var/lib/apt/lists/*

# install some tedious dependencies early
RUN R -e 'chooseCRANmirror(ind = 1); install.packages(c("remotes", "sessioninfo", "testthat", "purrr", "tidyr", "dplyr", "stringr"))'

# make COPASI dependencies
ADD https://github.com/copasi/copasi-dependencies/archive/master.tar.gz copasi-dependencies/
RUN cd copasi-dependencies/ \
	&& tar --strip 1 -xzf *

RUN cd copasi-dependencies/ \
	&& BUILD_DIR=${PWD}/tmp_linux_x64 \
		INSTALL_DIR=${PWD}/bin_linux_x64 \
		./createLinux.sh

# make COPASI
ADD https://github.com/copasi/COPASI/archive/release/Version-4.25.tar.gz COPASI/
RUN cd COPASI/ \
	&& tar --strip 1 -xzf *

ADD https://raw.githubusercontent.com/jpahle/CoRC/master/dev/copasi-source/CopasiVersion.h COPASI/copasi/

RUN mkdir corc_linux_x64/ \
	&& cd corc_linux_x64/ \
	&& cmake \
		-DCMAKE_BUILD_TYPE=Release \
		-DBUILD_GUI=OFF \
		-DBUILD_SE=OFF \
		-DENABLE_R=ON \
		-DR_USE_DYNAMIC_LOOKUP=ON \
		-DCOPASI_DEPENDENCY_DIR=../copasi-dependencies/bin_linux_x64/ \
		../COPASI/ \
	&& make binding_r_lib

# install corc
ADD https://github.com/jpahle/CoRC/archive/master.tar.gz CoRC/
RUN cd CoRC/ \
	&& tar --strip 1 -xzf *

RUN mkdir CoRC/inst/libs/ \
	&& cp corc_linux_x64/copasi/bindings/R/COPASI.so CoRC/inst/libs/ \
	&& cp corc_linux_x64/copasi/bindings/R/COPASI.R CoRC/R/swig_wrapper.R \
	&& R -e 'remotes::install_local("CoRC", INSTALL_opts = "--install-tests")' \
	&& rm -r copasi-dependencies/ COPASI/ corc_linux_x64/ CoRC/

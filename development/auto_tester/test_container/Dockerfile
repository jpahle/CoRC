FROM ubuntu:rolling

RUN apt-get update && apt-get install -y \
		build-essential \
		cmake \
		r-base-core \
		swig \
	&& rm -rf /var/lib/apt/lists/*

# install some tedious dependencies early
RUN R -e 'install.packages(c("remotes", "testthat", "purrr", "tidyr", "dplyr", "stringr"), repos = "https://cloud.r-project.org")'

ADD https://github.com/copasi/copasi-dependencies/archive/master.tar.gz copasi-dependencies/
RUN cd copasi-dependencies/ \
	&& tar --strip 1 -xzf *

RUN cd copasi-dependencies/ \
	&& BUILD_DIR=${PWD}/tmp_linux_x64 \
		INSTALL_DIR=${PWD}/bin_linux_x64 \
		./createLinux.sh

ADD https://github.com/copasi/COPASI/archive/develop.tar.gz COPASI/
RUN cd COPASI/ \
	&& tar --strip 1 -xzf *

ADD https://raw.githubusercontent.com/jpahle/CoRC/master/development/copasi-source/CopasiVersion.h COPASI/copasi/

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

ADD https://github.com/jpahle/CoRC/archive/master.tar.gz CoRC/
RUN cd CoRC/ \
	&& tar --strip 1 -xzf *

RUN mkdir CoRC/inst/libs/ \
	&& cp corc_linux_x64/copasi/bindings/R/COPASI.so CoRC/inst/libs/ \
	&& cp corc_linux_x64/copasi/bindings/R/COPASI.R CoRC/R/swig_wrapper.R

RUN R -e 'remotes::install_local("CoRC", INSTALL_opts = "--install-tests")'

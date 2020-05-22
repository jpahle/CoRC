# syntax = docker/dockerfile:1.0

FROM ubuntu:20.04 AS r_ubuntu_20_04
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
	r-base-core \
	&& rm -rf /var/lib/apt/lists/*
ENV R_OPTS "options(repos = c(CRAN = 'https://cloud.r-project.org/')); options(Ncpus = as.integer(system2('nproc', stdout = TRUE))); options(warn = 2);"
ENV R_DEPS_CMD "install.packages(c('remotes', 'testthat', 'sessioninfo', 'purrr', 'dplyr', 'tidyr', 'stringr', 'isnullptr'));"
RUN Rscript -e "$R_OPTS $R_DEPS_CMD"
WORKDIR /work

FROM ubuntu:18.04 AS r_ubuntu_18_04
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
	r-base-core \
	&& rm -rf /var/lib/apt/lists/*
ENV R_OPTS "options(repos = c(CRAN = 'https://cloud.r-project.org/')); options(Ncpus = as.integer(system2('nproc', stdout = TRUE))); options(warn = 2);"
ENV R_DEPS_CMD "install.packages(c('remotes', 'testthat', 'sessioninfo', 'purrr', 'dplyr', 'tidyr', 'stringr', 'isnullptr'));"
RUN Rscript -e "$R_OPTS $R_DEPS_CMD"
WORKDIR /work

FROM ubuntu:16.04 AS r_ubuntu_16_04
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
	r-base-core \
	&& rm -rf /var/lib/apt/lists/*
ENV R_OPTS "options(repos = c(CRAN = 'https://cloud.r-project.org/')); options(Ncpus = as.integer(system2('nproc', stdout = TRUE))); options(warn = 2);"
ENV R_DEPS_CMD "install.packages(c('remotes', 'testthat', 'sessioninfo', 'purrr', 'dplyr', 'tidyr', 'stringr', 'isnullptr'));"
RUN Rscript -e "$R_OPTS $R_DEPS_CMD"
ENV UNSTABLE_RSCRIPT=true
WORKDIR /work

FROM centos:8 AS r_centos_8
RUN yum install -y epel-release yum-utils \
	&& yum clean all \
	&& yum config-manager --set-enabled PowerTools \
	&& yum install -y R-core-devel \
	&& yum clean all
ENV R_OPTS "options(repos = c(CRAN = 'https://cloud.r-project.org/')); options(Ncpus = as.integer(system2('nproc', stdout = TRUE))); options(warn = 2);"
ENV R_DEPS_CMD "install.packages(c('remotes', 'testthat', 'sessioninfo', 'purrr', 'dplyr', 'tidyr', 'stringr', 'isnullptr'));"
RUN Rscript -e "$R_OPTS $R_DEPS_CMD"
WORKDIR /work

FROM centos:7 AS r_centos_7
RUN yum install -y epel-release \
	&& yum install -y R-core-devel \
	&& yum clean all
# prevent errors when package docs are installed
RUN Rscript -e 'x <- file.path(R.home("doc"), "html"); if (!file.exists(x)) {dir.create(x, recursive=TRUE); file.copy(system.file("html/R.css", package="stats"), x)}'
ENV R_OPTS "options(repos = c(CRAN = 'https://cloud.r-project.org/')); options(Ncpus = as.integer(system2('nproc', stdout = TRUE))); options(warn = 2);"
ENV R_DEPS_CMD "install.packages(c('remotes', 'testthat', 'sessioninfo', 'purrr', 'dplyr', 'tidyr', 'stringr', 'isnullptr'));"
RUN Rscript -e "$R_OPTS $R_DEPS_CMD"
WORKDIR /work

FROM centos:6 AS r_centos_6
RUN yum install -y epel-release \
	&& yum install -y R-core-devel \
	&& yum clean all
# prevent errors when package docs are installed
RUN Rscript -e 'x <- file.path(R.home("doc"), "html"); if (!file.exists(x)) {dir.create(x, recursive=TRUE); file.copy(system.file("html/R.css", package="stats"), x)}'
ENV R_OPTS "options(repos = c(CRAN = 'http://cloud.r-project.org/')); options(Ncpus = as.integer(system2('nproc', stdout = TRUE))); options(warn = 2);"
ENV R_DEPS_CMD "install.packages(c('remotes', 'testthat', 'sessioninfo', 'purrr', 'dplyr', 'tidyr', 'stringr', 'isnullptr'));"
RUN Rscript -e "$R_OPTS $R_DEPS_CMD"
WORKDIR /work

FROM ubuntu:20.04 AS rbuilder
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
	build-essential \
	cmake \
	curl \
	automake \
	bison \
	gfortran \
	libcurl4-openssl-dev \
	libpcre2-dev\
	libpcre3-dev \
	libreadline-dev \
	liblzma-dev \
	zlib1g-dev \
	libbz2-dev \
	&& rm -rf /var/lib/apt/lists/*
ENV R_OPTS "options(repos = c(CRAN = 'https://cloud.r-project.org/')); options(Ncpus = as.integer(system2('nproc', stdout = TRUE))); options(warn = 2);"
ENV R_DEPS_CMD "install.packages(c('remotes', 'testthat', 'sessioninfo', 'purrr', 'dplyr', 'tidyr', 'stringr', 'isnullptr'));"
WORKDIR /work

# R-latest matches latest alpha, beta or rc
FROM rbuilder AS r_latest
RUN curl -L https://cran.r-project.org/src/base-prerelease/R-latest.tar.gz | tar -xz \
	&& cd R-*/ \
	&& ./configure \
		--prefix=/opt/R/ \
		--enable-R-shlib \
		--without-recommended-packages --with-x=no --with-aqua=no \
		--with-tcltk=no --with-ICU=no --disable-java --disable-openmp --disable-nls \
		--disable-R-profiling --disable-BLAS-shlib --with-libpng=no --with-jpeglib=no --with-libtiff=no \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r R-*/
RUN ln -s /opt/R/bin/Rscript /opt/R/bin/R /usr/local/bin/
RUN Rscript -e "$R_OPTS $R_DEPS_CMD"

FROM rbuilder AS r_4_0
RUN curl -L https://cran.r-project.org/src/base/R-4/R-4.0.0.tar.gz | tar -xz \
	&& cd R-*/ \
	&& ./configure \
		--prefix=/opt/R/ \
		--enable-R-shlib \
		--without-recommended-packages --with-x=no --with-aqua=no \
		--with-tcltk=no --with-ICU=no --disable-java --disable-openmp --disable-nls \
		--disable-R-profiling --disable-BLAS-shlib --with-libpng=no --with-jpeglib=no --with-libtiff=no \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r R-*/
RUN ln -s /opt/R/bin/Rscript /opt/R/bin/R /usr/local/bin/
RUN Rscript -e "$R_OPTS $R_DEPS_CMD"

FROM rbuilder AS r_3_6
RUN curl -L https://cran.r-project.org/src/base/R-3/R-3.6.3.tar.gz | tar -xz \
	&& cd R-*/ \
	&& ./configure \
		--prefix=/opt/R/ \
		--enable-R-shlib \
		--without-recommended-packages --with-x=no --with-aqua=no \
		--with-tcltk=no --with-ICU=no --disable-java --disable-openmp --disable-nls \
		--disable-R-profiling --disable-BLAS-shlib --with-libpng=no --with-jpeglib=no --with-libtiff=no \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r R-*/
RUN ln -s /opt/R/bin/Rscript /opt/R/bin/R /usr/local/bin/
RUN Rscript -e "$R_OPTS $R_DEPS_CMD"

FROM rbuilder AS r_3_5
RUN curl -L https://cran.r-project.org/src/base/R-3/R-3.5.3.tar.gz | tar -xz \
	&& cd R-*/ \
	&& ./configure \
		--prefix=/opt/R/ \
		--enable-R-shlib \
		--without-recommended-packages --with-x=no --with-aqua=no \
		--with-tcltk=no --with-ICU=no --disable-java --disable-openmp --disable-nls \
		--disable-R-profiling --disable-BLAS-shlib --with-libpng=no --with-jpeglib=no --with-libtiff=no \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r R-*/
RUN ln -s /opt/R/bin/Rscript /opt/R/bin/R /usr/local/bin/
RUN Rscript -e "$R_OPTS $R_DEPS_CMD"

FROM rbuilder AS r_3_4
RUN curl -L https://cran.r-project.org/src/base/R-3/R-3.4.4.tar.gz | tar -xz \
	&& cd R-*/ \
	&& ./configure \
		--prefix=/opt/R/ \
		--enable-R-shlib \
		--without-recommended-packages --with-x=no --with-aqua=no \
		--with-tcltk=no --with-ICU=no --disable-java --disable-openmp --disable-nls \
		--disable-R-profiling --disable-BLAS-shlib --with-libpng=no --with-jpeglib=no --with-libtiff=no \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r R-*/
RUN ln -s /opt/R/bin/Rscript /opt/R/bin/R /usr/local/bin/
RUN Rscript -e "$R_OPTS $R_DEPS_CMD"

FROM rbuilder AS r_3_3
RUN curl -L https://cran.r-project.org/src/base/R-3/R-3.3.3.tar.gz | tar -xz \
	&& cd R-*/ \
	&& ./configure \
		--prefix=/opt/R/ \
		--enable-R-shlib \
		--without-recommended-packages --with-x=no --with-aqua=no \
		--with-tcltk=no --with-ICU=no --disable-java --disable-openmp --disable-nls \
		--disable-R-profiling --disable-BLAS-shlib --with-libpng=no --with-jpeglib=no --with-libtiff=no \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r R-*/
RUN ln -s /opt/R/bin/Rscript /opt/R/bin/R /usr/local/bin/
RUN Rscript -e "$R_OPTS $R_DEPS_CMD"
ENV UNSTABLE_RSCRIPT=true

FROM rbuilder AS r_3_2
RUN curl -L https://cran.r-project.org/src/base/R-3/R-3.2.5.tar.gz | tar -xz \
	&& cd R-*/ \
	&& ./configure \
		--prefix=/opt/R/ \
		--enable-R-shlib \
		--without-recommended-packages --with-x=no --with-aqua=no \
		--with-tcltk=no --with-ICU=no --disable-java --disable-openmp --disable-nls \
		--disable-R-profiling --disable-BLAS-shlib --with-libpng=no --with-jpeglib=no --with-libtiff=no \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r R-*/
RUN ln -s /opt/R/bin/Rscript /opt/R/bin/R /usr/local/bin/
RUN Rscript -e "$R_OPTS $R_DEPS_CMD"
ENV UNSTABLE_RSCRIPT=true

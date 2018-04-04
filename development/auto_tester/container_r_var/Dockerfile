FROM ubuntu:18.04

RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
	build-essential \
	cmake \
	curl \
	automake \
	bison \
	gfortran \
	libcurl4-openssl-dev \
	libpcre3-dev \
	libreadline-dev \
	liblzma-dev \
	zlib1g-dev \
	libbz2-dev \
	r-base \
	&& rm -rf /var/lib/apt/lists/*

WORKDIR /prep

# install R-3.1.3 to /opt/R/3.1/
RUN curl -L https://cran.r-project.org/src/base/R-3/R-3.1.3.tar.gz | tar -xz \
	&& cd R-*/ \
 	&& ./configure \
		--prefix=/opt/R/3.1 \
		--enable-R-shlib \
		--without-recommended-packages --with-x=no --with-aqua=no \
		--with-tcltk=no --with-ICU=no --disable-java --disable-openmp --disable-nls \
		--disable-R-profiling --disable-BLAS-shlib --with-libpng=no --with-jpeglib=no --with-libtiff=no \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r R-*/

# install R-3.2.5 to /opt/R/3.2/
RUN curl -L https://cran.r-project.org/src/base/R-3/R-3.2.5.tar.gz | tar -xz \
	&& cd R-*/ \
 	&& ./configure \
		--prefix=/opt/R/3.2 \
		--enable-R-shlib \
		--without-recommended-packages --with-x=no --with-aqua=no \
		--with-tcltk=no --with-ICU=no --disable-java --disable-openmp --disable-nls \
		--disable-R-profiling --disable-BLAS-shlib --with-libpng=no --with-jpeglib=no --with-libtiff=no \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r R-*/

# install R-3.3.3 to /opt/R/3.3/
RUN curl -L https://cran.r-project.org/src/base/R-3/R-3.3.3.tar.gz | tar -xz \
	&& cd R-*/ \
 	&& ./configure \
		--prefix=/opt/R/3.3 \
		--enable-R-shlib \
		--without-recommended-packages --with-x=no --with-aqua=no \
		--with-tcltk=no --with-ICU=no --disable-java --disable-openmp --disable-nls \
		--disable-R-profiling --disable-BLAS-shlib --with-libpng=no --with-jpeglib=no --with-libtiff=no \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r R-*/

# install R-3.4.5 to /opt/R/3.4/
RUN curl -L https://cran.r-project.org/src/base/R-3/R-3.4.4.tar.gz | tar -xz \
	&& cd R-*/ \
 	&& ./configure \
		--prefix=/opt/R/3.4 \
		--enable-R-shlib \
		--without-recommended-packages --with-x=no --with-aqua=no \
		--with-tcltk=no --with-ICU=no --disable-java --disable-openmp --disable-nls \
		--disable-R-profiling --disable-BLAS-shlib --with-libpng=no --with-jpeglib=no --with-libtiff=no \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r R-*/

# install R-latest to /opt/R/latest/
# this matches latest alpha, beta or rc
RUN curl -L https://cran.r-project.org/src/base-prerelease/R-latest.tar.gz | tar -xz \
	&& cd R-*/ \
 	&& ./configure \
		--prefix=/opt/R/latest \
		--enable-R-shlib \
		--without-recommended-packages --with-x=no --with-aqua=no \
		--with-tcltk=no --with-ICU=no --disable-java --disable-openmp --disable-nls \
		--disable-R-profiling --disable-BLAS-shlib --with-libpng=no --with-jpeglib=no --with-libtiff=no \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r R-*/

ENV R_OPTS "options(repos = c(CRAN = 'https://cloud.r-project.org/')); options(Ncpus = 4);"
ENV R_32_OPTS "options(repos = c(CRAN = 'https://cloud.r-project.org/'));"
ENV R_31_OPTS "options(repos = c(CRAN = 'http://cloud.r-project.org/'));"

# prepare R for CoRC
RUN Rscript                   -e "$R_OPTS install.packages(c('remotes', 'testthat', 'sessioninfo', 'purrr', 'dplyr', 'tidyr', 'stringr', 'isnullptr'))"
RUN /opt/R/latest/bin/Rscript -e "$R_OPTS install.packages(c('remotes', 'testthat', 'sessioninfo', 'purrr', 'dplyr', 'tidyr', 'stringr', 'isnullptr'))"
RUN /opt/R/3.4/bin/Rscript    -e "$R_OPTS install.packages(c('remotes', 'testthat', 'sessioninfo', 'purrr', 'dplyr', 'tidyr', 'stringr', 'isnullptr'))"
RUN /opt/R/3.3/bin/Rscript    -e "$R_OPTS install.packages(c('remotes', 'testthat', 'sessioninfo', 'purrr', 'dplyr', 'tidyr', 'stringr', 'isnullptr'))"
RUN /opt/R/3.2/bin/Rscript    -e "$R_32_OPTS install.packages(c('remotes', 'testthat', 'sessioninfo', 'purrr', 'dplyr', 'tidyr', 'stringr', 'isnullptr'))"
RUN /opt/R/3.1/bin/Rscript    -e "$R_31_OPTS install.packages(c('remotes', 'testthat', 'sessioninfo', 'purrr', 'dplyr', 'tidyr', 'stringr', 'isnullptr'))"

# fix issues because of old R versions
RUN /opt/R/3.1/bin/Rscript    -e "$R_31_OPTS install.packages('curl'); remotes::install_version('tidyr', '0.7.2')"

WORKDIR /work

# include the test script
COPY full_test.sh .

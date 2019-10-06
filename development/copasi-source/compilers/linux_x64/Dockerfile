# manylinux supplies a base centos5 with recent openssl etc.
FROM quay.io/pypa/manylinux2010_x86_64

# Make sure to adjust the R version further below

RUN yum install -y \
	# R depencency
	readline-devel.x86_64 \
	&& yum clean all

WORKDIR /prep

# Curl depencency, current
RUN curl -L https://github.com/openssl/openssl/archive/OpenSSL_1_1_1c.tar.gz | tar -xz \
	&& cd openssl-*/ \
	&& ./config \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r openssl-*/

# R depencency, current
RUN curl -L https://github.com/curl/curl/releases/download/curl-7_65_3/curl-7.65.3.tar.gz | tar -xz \
	&& cd curl-*/ \
	&& ./configure --with-ssl \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r curl-*/

# R depencency, current
# RUN curl -L https://sourceforge.net/projects/bzip2/files/bzip2-1.0.6.tar.gz/download | tar -xz \
RUN curl -L https://github.com/enthought/bzip2-1.0.6/archive/master.tar.gz | tar -xz \
	&& cd bzip2-*/ \
	# force -fPIC for bzip2
	&& sed -i "s/^CC=gcc$/CC=gcc -fPIC/" Makefile \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r bzip2-*/

# R depencency, current
RUN curl -L https://github.com/madler/zlib/archive/v1.2.11.tar.gz | tar -xz \
	&& cd zlib-*/ \
	&& ./configure \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r zlib-*/

# R depencency, current
RUN curl -L https://tukaani.org/xz/xz-5.2.4.tar.gz | tar -xz \
	&& cd xz-*/ \
	&& ./configure \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r xz-*/

# R depencency, current
# RUN curl -L ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/pcre-8.43.tar.gz | tar -xz \
RUN curl -L https://ftp.pcre.org/pub/pcre/pcre-8.43.tar.gz | tar -xz \
	&& cd pcre-*/ \
	&& ./configure --enable-utf --enable-unicode-properties --enable-jit --disable-cpp \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r pcre-*/

# COPASI depencency, current
RUN curl -L http://sourceforge.net/projects/libuuid/files/libuuid-1.0.3.tar.gz/download | tar -xz \
	&& cd libuuid-*/ \
	&& ./configure \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r libuuid-*/

# COPASI R bindings depencency, keep 3.0.12
# RUN curl -L "http://downloads.sourceforge.net/project/swig/swig/swig-3.0.12/swig-3.0.12.tar.gz" | tar -zx \
# get from github instead for reliability
RUN curl -L https://github.com/swig/swig/archive/rel-3.0.12.tar.gz | tar -xz \
	&& cd swig-rel-*/ \
	&& ./autogen.sh \
	&& ./configure --disable-dependency-tracking \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r swig-rel-*/

# R depencency, current
RUN curl -L https://github.com/Kitware/CMake/releases/download/v3.15.2/cmake-3.15.2.tar.gz | tar -xz \
	&& cd cmake-*/ \
	&& ./bootstrap \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r cmake-*/

# R
# for latest R release
RUN curl -L https://cran.r-project.org/src/base/R-latest.tar.gz | tar -xz \
# for latest R alpha, beta, RC
# RUN curl -L https://cran.r-project.org/src/base-prerelease/R-latest.tar.gz | tar -xz \
	&& cd R-*/ \
 	&& ./configure \
		--enable-R-shlib \
		--without-recommended-packages --with-x=no --with-aqua=no \
		--with-tcltk=no --with-ICU=no --disable-java --disable-openmp --disable-nls --disable-largefile \
		--disable-R-profiling --disable-BLAS-shlib --with-libpng=no --with-jpeglib=no --with-libtiff=no \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r R-*/

WORKDIR /work

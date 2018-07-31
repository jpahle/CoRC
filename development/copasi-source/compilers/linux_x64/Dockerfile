# manylinux supplies a base centos5 with recent openssl etc.
FROM quay.io/pypa/manylinux1_x86_64

# Make sure to adjust the R version further below

RUN yum install -y \
	# R depencency
	readline-devel.x86_64 \
	&& yum clean all

WORKDIR /prep

# R depencency, current
RUN curl -L http://www.bzip.org/1.0.6/bzip2-1.0.6.tar.gz | tar -xz \
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
RUN curl -L ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/pcre-8.42.tar.gz | tar -xz \
	&& cd pcre-*/ \
	&& ./configure --enable-utf --enable-unicode-properties --enable-jit --disable-cpp \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r pcre-*/

# COPASI depencency, current
RUN curl -L https://cmake.org/files/v3.11/cmake-3.11.4.tar.gz | tar -xz \
	&& cd cmake-*/ \
	&& ./bootstrap \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r cmake-*/

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

# R
# for latest R release
 RUN curl -L https://cran.r-project.org/src/base/R-latest.tar.gz | tar -xz \
# for latest R alpha, beta, RC
#RUN curl -L https://cran.r-project.org/src/base-prerelease/R-latest.tar.gz | tar -xz \
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

# remove some libs so they dont interfere with the copasi / dependency building
RUN yum remove -y \
	expat-devel

WORKDIR /work

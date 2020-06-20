# manylinux supplies a base centos5 with recent openssl etc.
FROM quay.io/pypa/manylinux2010_x86_64

# This is the main linux compiler
ENV LINUX_TAG=linux_x86_64

RUN yum update -y \
	&& yum install -y \
		openssl-devel \
		R-core-devel \
	&& yum clean all

WORKDIR /prep/

# libuuid, keep 1.0.3
RUN curl -L http://bqfrank.spdns.org/Files/libuuid-1.0.3.tar.gz | tar -xz \
  && cd libuuid-*/ \
  && ./configure --disable-dependency-tracking --with-pic --enable-static=yes --enable-shared=no \
  && make clean \
  && make -j$(nproc) \
  && make install \
  && rm /usr/local/lib/libuuid.la \
  && cd ../ \
  && rm -r libuuid-*/

# COPASI dependency, current
RUN curl -L https://cmake.org/files/v3.17/cmake-3.17.2.tar.gz | tar -xz \
	&& cd cmake-*/ \
	&& ./bootstrap \
	&& make -j$(nproc) \
	&& make install \
	&& cd ../ \
	&& rm -r cmake-*/

# COPASI R bindings dependency, keep 3.0.12
# RUN curl -L "http://downloads.sourceforge.net/project/swig/swig/swig-3.0.12/swig-3.0.12.tar.gz" | tar -zx \
# get from github instead for reliability
RUN curl -L https://github.com/swig/swig/archive/rel-3.0.12.tar.gz | tar -xz \
	&& cd swig-rel-*/ \
	&& ./autogen.sh \
	&& ./configure --disable-dependency-tracking \
	&& make -j$(nproc) \
	&& make install \
	&& cd ../ \
	&& rm -r swig-rel-*/

WORKDIR /work/

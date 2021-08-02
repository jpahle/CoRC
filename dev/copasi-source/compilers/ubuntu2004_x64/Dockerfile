FROM ubuntu:20.04

ENV LINUX_TAG=ubuntu2004_x86_64

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y \
	curl \
	build-essential \
	automake \
	cmake \
	bison \
	&& rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y \
	r-base-dev \
	&& rm -rf /var/lib/apt/lists/*

ENV R_INCLUDE_DIRS=/usr/share/R/include/
ENV R_LIB=/usr/lib/R/lib/libR.so

WORKDIR /prep/

# COPASI dependency, current
# RUN curl -L https://cmake.org/files/v3.17/cmake-3.17.0.tar.gz | tar -xz \
# 	&& cd cmake-*/ \
# 	&& ./bootstrap \
# 	&& make -j$(nproc) \
# 	&& make install \
# 	&& cd ../ \
# 	&& rm -r cmake-*/

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

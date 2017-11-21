FROM debian:stretch-slim

# base deps
RUN apt-get update && apt-get install -y \
	build-essential \
	automake \
	cmake \
	pkg-config \
	curl \
	wget \
	git \
	&& rm -rf /var/lib/apt/lists/*

# compilation resource deps
RUN apt-get update && apt-get install -y \
	libpcre2-dev \
	libpcre++-dev \
	bison \
	p7zip-full \
	cpio \
	&& rm -rf /var/lib/apt/lists/*

# osxcross deps
RUN apt-get update && apt-get install -y \
	clang \
	patch \
	libxml2-dev \
	llvm-dev \
	fuse \
	&& rm -rf /var/lib/apt/lists/*

WORKDIR /work/

# get R package for Mac
RUN mkdir rtmp/ \
	&& cd rtmp/ \
	&& wget https://cran.r-project.org/bin/macosx/R-3.3.3.pkg \
	&& 7z x R-3.3.3.pkg \
	&& cat r.pkg/Payload | gunzip -dc | cpio -i \
	&& cp -r R.framework/Resources/ /opt/R-resources/ \
	&& cd ../ \
	&& rm -r rtmp/

# install swig
# RUN wget -O- "http://downloads.sourceforge.net/project/swig/swig/swig-3.0.12/swig-3.0.12.tar.gz" | tar zx \
# get from github instead for reliability
RUN wget -O- https://github.com/swig/swig/archive/rel-3.0.12.tar.gz | tar xz \
	&& cd swig-rel-3.0.12/ \
	&& ./autogen.sh \
	&& ./configure --disable-dependency-tracking \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r swig-rel-3.0.12/

# setup osxcross
ENV OSXCROSS_REPO="tpoechtrager/osxcross" \
    OSXCROSS_REVISION="1a1733a773fe26e7b6c93b16fbf9341f22fac831" \
    DARWIN_SDK_VERSION="10.9" \
    DARWIN_OSX_VERSION_MIN="10.8" \
	OSXCROSS_PATH="/opt/osxcross"

# install osxcross
RUN mkdir -p ${OSXCROSS_PATH} \
	&& cd ${OSXCROSS_PATH} \
	&& wget -O- "https://codeload.github.com/${OSXCROSS_REPO}/tar.gz/${OSXCROSS_REVISION}" | tar zx --strip=1 \
	&& wget -P tarballs/ "https://github.com/phracker/MacOSX-SDKs/releases/download/10.13/MacOSX${DARWIN_SDK_VERSION}.sdk.tar.xz" \
	&& OSX_VERSION_MIN=${DARWIN_OSX_VERSION_MIN} OSXCROSS_TARGET=darwin13 UNATTENDED=1 ./build.sh

# force build env
ENV PATH ${OSXCROSS_PATH}/target/bin:$PATH
ENV CC=o64-clang CXX=o64-clang++

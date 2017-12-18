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
	
#Build arguments
ARG osxcross_repo="tpoechtrager/osxcross"
ARG osxcross_revision="1a1733a773fe26e7b6c93b16fbf9341f22fac831"
ARG osxcross_path="/opt/osxcross"
ARG darwin_sdk_version="10.10"
ARG darwin_osx_version_min="10.8"
ARG darwin_version="14"
ARG darwin_sdk_url="https://www.dropbox.com/s/yfbesd249w10lpc/MacOSX${darwin_sdk_version}.sdk.tar.xz"

# ENV available in docker image
ENV OSXCROSS_REPO="${osxcross_repo}" \
    OSXCROSS_REVISION="${osxcross_revision}" \
	OSXCROSS_PATH="${osxcross_path}" \
    DARWIN_SDK_VERSION="${darwin_sdk_version}" \
    DARWIN_VERSION="${darwin_version}" \
    DARWIN_OSX_VERSION_MIN="${darwin_osx_version_min}" \
    DARWIN_SDK_URL="${darwin_sdk_url}" \
    MACOSX_DEPLOYMENT_TARGET="${darwin_osx_version_min}"

# install osxcross
RUN mkdir -p ${OSXCROSS_PATH} \
	&& cd ${OSXCROSS_PATH} \
	&& wget -O- "https://codeload.github.com/${OSXCROSS_REPO}/tar.gz/${OSXCROSS_REVISION}" | tar zx --strip=1 \
	&& wget -P tarballs/ ${DARWIN_SDK_URL} \
	&& OSX_VERSION_MIN=${DARWIN_OSX_VERSION_MIN} UNATTENDED=1 ./build.sh \
	&& ./build_compiler_rt.sh

# force build env
ENV PATH ${OSXCROSS_PATH}/target/bin:$PATH
ENV CC=o64-clang CXX=o64-clang++

COPY toolchain-apple-darwin.cmake /opt/

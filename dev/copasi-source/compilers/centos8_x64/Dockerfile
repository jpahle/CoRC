FROM centos:8

ENV LINUX_TAG=centos8_x86_64

RUN yum install -y epel-release yum-utils \
	&& yum config-manager --set-enabled PowerTools \
	&& yum install -y \
		R-core-devel \
	&& yum clean all

RUN yum upgrade -y \
	&& yum groupinstall -y 'Development Tools' \
	&& yum install -y \
		cmake \
		openssl-devel \
		libuuid-devel \
	&& yum clean all

WORKDIR /prep/

# COPASI dependency, current
# RUN curl -L https://cmake.org/files/v3.17/cmake-3.17.0.tar.gz | tar -xz \
# 	&& cd cmake-*/ \
# 	&& ./bootstrap \
# 	&& make -j$(nproc) \
# 	&& make install \
# 	&& cd ../ \
# 	&& rm -r cmake-*/

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

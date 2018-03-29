# manylinux supplies a base centos5 with recent openssl etc.
FROM quay.io/pypa/manylinux1_x86_64

RUN yum install -y \
	pcre-devel.x86_64 \
	R-core-devel.x86_64 \
	&& yum clean all

# install recent cmake
RUN curl -L https://cmake.org/files/v3.11/cmake-3.11.1.tar.gz | tar -xz \
	&& cd cmake-3.11.1/ \
	&& ./bootstrap \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r cmake-3.11.1/

# install recent swig
# RUN curl -L "http://downloads.sourceforge.net/project/swig/swig/swig-3.0.12/swig-3.0.12.tar.gz" | tar -zx \
# get from github instead for reliability
RUN curl -L https://github.com/swig/swig/archive/rel-3.0.12.tar.gz | tar -xz \
	&& cd swig-rel-3.0.12/ \
	&& ./autogen.sh \
	&& ./configure --disable-dependency-tracking \
	&& make \
	&& make install \
	&& cd ../ \
	&& rm -r swig-rel-3.0.12/

WORKDIR /work

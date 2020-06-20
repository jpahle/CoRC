FROM centos:6

ENV LINUX_TAG=centos6_x86_64

RUN yum install -y centos-release-scl \
	&& yum install -y \
		devtoolset-8 \
	&& yum clean all

RUN yum install -y epel-release \
	&& yum install -y \
		R-core-devel \
	&& yum clean all

RUN yum upgrade -y \
	&& yum groupinstall -y 'Development Tools' \
	&& yum install -y \
		openssl-devel \
	&& yum clean all

SHELL [ "/usr/bin/scl", "enable", "devtoolset-8", "--", "/bin/sh", "-c" ]
ENTRYPOINT [ "/usr/bin/scl", "enable", "devtoolset-8", "--" ]
CMD [ "/bin/bash" ]

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
RUN curl -L https://cmake.org/files/v3.17/cmake-3.17.0.tar.gz | tar -xz \
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

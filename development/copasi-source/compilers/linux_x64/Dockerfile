# manylinux supplies a base centos5 with recent openssl etc.
FROM quay.io/pypa/manylinux2010_x86_64

# Make sure to adjust the R version further below

RUN yum update \
	&& yum install -y \
		cmake3 \
		libuuid-devel \
		R-core-devel \
	&& yum clean all

WORKDIR /prep

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

WORKDIR /work

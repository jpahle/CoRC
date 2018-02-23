# dockcross manylinux supplies a base centos5 with recent cmake openssl etc.
FROM dockcross/manylinux-x64

RUN yum install -y \
	pcre-devel.x86_64 \
	R-core-devel.x86_64 \
	&& yum clean all

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

# dockcross is conceived to be run as an executable but we just want to enter to bash and run a script
ENTRYPOINT []

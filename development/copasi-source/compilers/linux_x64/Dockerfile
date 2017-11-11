FROM dockcross/manylinux-x64

RUN yum install -y pcre-devel.x86_64 R.x86_64 \
	&& yum clean all

RUN wget -qO- "http://downloads.sourceforge.net/project/swig/swig/swig-3.0.12/swig-3.0.12.tar.gz" | tar zx \
	&& cd swig-3.0.12 \
	&& ./configure --disable-dependency-tracking \
	&& make \
	&& make install \
	&& cd .. \
	&& rm -rf swig-3.0.12

ENV DEFAULT_DOCKCROSS_IMAGE corc_compiler_linux_x64

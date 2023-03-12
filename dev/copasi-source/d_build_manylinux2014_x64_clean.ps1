docker build --pull -t corc_compiler_manylinux2014_x64 compilers/manylinux2014_x64

docker run --rm -v ${PWD}:/work corc_compiler_manylinux2014_x64 ./build_linux_x64_clean.sh

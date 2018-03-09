docker build -t corc_compiler_linux_x64 compilers/linux_x64

docker run --rm -v ${PWD}:/work corc_compiler_linux_x64 ./build_linux_x64_quick.sh

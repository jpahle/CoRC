docker build compilers/linux_x64
$id=&"docker build -q compilers/linux_x64"

docker run --rm -v ${PWD}:/work ${id} bash ./build_linux_x64_clean.sh

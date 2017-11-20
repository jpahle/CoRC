#!/bin/bash
set -e
set -x

chmod +x ./build_linux_x64_quick.sh

docker build compilers/linux_x64
containerid=$(docker build -q compilers/linux_x64)
user=$UID:$(id -g $USER)

docker run --rm -v $PWD:/work -u $user $containerid ./build_linux_x64_quick.sh

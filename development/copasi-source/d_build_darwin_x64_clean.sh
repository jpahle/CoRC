#!/bin/bash
set -e
set -x

chmod +x ./build_darwin_x64_clean.sh

docker build compilers/darwin_x64
containerid=$(docker build -q compilers/darwin_x64)
user=$UID:$(id -g $USER)

docker run --rm -v $PWD:/work -u $user $containerid ./build_darwin_x64_clean.sh

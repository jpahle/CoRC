#!/bin/bash
set -e
set -x

docker build -t corc_test_r_var container_r_var

docker run -v $PWD/../..:/work/CoRC corc_test_r_var ./full_test.sh

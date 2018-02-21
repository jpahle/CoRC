#!/bin/bash
set -e
set -x

docker build -t corc_auto_tester test_container 

docker run corc_auto_tester R -e "testthat::test_dir(\"CoRC/tests/\")"

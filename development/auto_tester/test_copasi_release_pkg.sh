#!/bin/bash
set -e
set -x

docker build -t corc_test_release container_copasi_release

docker run corc_test_release R -e 'sessioninfo::session_info("CoRC"); testthat::test_package("CoRC", reporter = "progress")'

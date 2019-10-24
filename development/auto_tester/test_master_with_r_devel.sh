#!/bin/bash
set -e
set -x

docker build --pull -t corc_test_r_devel container_r_devel

docker run corc_test_r_devel R -e 'sessioninfo::session_info("CoRC"); testthat::test_package("CoRC", reporter = "progress")'


#!/bin/bash
set -e
set -x

docker build -t corc_test_r_devel container_r_devel

docker run corc_test_r_devel R -e 'devtools::session_info("CoRC"); testthat::test_package("CoRC", reporter = "progress")'

#!/bin/bash
set -e
set -x

docker build --pull -t corc_test_r_latest container_r_latest

docker run corc_test_r_latest R -e 'sessioninfo::session_info("CoRC"); testthat::test_package("CoRC", reporter = "progress")'


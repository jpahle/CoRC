#!/bin/bash
set -e
set -x

docker build -t corc_test_dev container_copasi_dev

docker run corc_test_dev R -e 'sessioninfo::session_info("CoRC"); source(system.file("tests", "other", "test_biomodels.R", package = "CoRC"))'

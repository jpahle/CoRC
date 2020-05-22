#!/bin/bash
set -e
set -x

export DOCKER_BUILDKIT=1
# export BUILDKIT_PROGRESS=plain
docker build -t corc_test_r_${R_VARIANT} --target r_${R_VARIANT} .

TEST=${TEST:=full}

docker run \
    -v ${PWD}/../../../:/work/CoRC \
    -v ${PWD}/${TEST}_test.sh:/work/${TEST}_test.sh \
    corc_test_r_${R_VARIANT} \
    ./${TEST}_test.sh \

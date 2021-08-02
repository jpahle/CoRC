#!/bin/bash

export TEST=full
# export TEST=quick
# export TEST=biomodels

# R_VARIANT=ubuntu_20_04 ./test_lib_with_rX.sh
R_VARIANTS=(
	ubuntu_20_04
	ubuntu_18_04
	ubuntu_16_04
	centos_8
	centos_7
	centos_6
	fedora_34
	fedora_32
	fedora_30
	latest
	4_1
	4_0
	3_6
	3_5
	3_4
	3_3
	3_2
)

# optionally rebuild all first
# export DOCKER_BUILDKIT=1
# export BUILDKIT_PROGRESS=plain
# for var in "${R_VARIANTS[@]}"; do
# 	docker build --pull -t corc_test_r_${var} --target r_${var} container_r_var/
# done

# run the quick or full test
for var in "${R_VARIANTS[@]}"; do
	echo "Testing ${var}... see r_${var}.log"
	(cd container_r_var/ && R_VARIANT=${var} ./test_lib_with_rX.sh) &> r_${var}.log
	echo $?
done

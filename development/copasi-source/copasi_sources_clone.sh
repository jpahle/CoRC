#!/bin/bash
set +e
set -x

COPASI_TAG=$(cat COPASI_TAG)
COPASI_DEPENDENCIES_TAG=$(cat COPASI_DEPENDENCIES_TAG)


rm -r COPASI/ copasi-dependencies/


git clone https://github.com/jonasfoe/copasi-dependencies.git
cd copasi-dependencies/
git checkout ${COPASI_DEPENDENCIES_TAG}
cd ../

git clone https://github.com/jonasfoe/COPASI.git
cd COPASI/
git checkout ${COPASI_TAG}
cd ../

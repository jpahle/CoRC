#!/bin/bash
set +e
set -x

COPASI_TAG=$(cat COPASI_TAG)
COPASI_DEPENDENCIES_TAG=$(cat COPASI_DEPENDENCIES_TAG)


rm -r COPASI/ copasi-dependencies/
mkdir COPASI/ copasi-dependencies/


cd copasi-dependencies/
curl -L https://github.com/jonasfoe/copasi-dependencies/archive/${COPASI_DEPENDENCIES_TAG}.tar.gz | tar -xz --strip 1
cd ../

cd COPASI/
curl -L https://github.com/jonasfoe/COPASI/archive/${COPASI_TAG}.tar.gz | tar -xz --strip 1
cd ../

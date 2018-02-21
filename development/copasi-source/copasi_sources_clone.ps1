$COPASI_TAG = Get-Content COPASI_TAG
$COPASI_DEPENDENCIES_TAG = Get-Content COPASI_DEPENDENCIES_TAG


rm COPASI\ -Recurse -Force -ErrorAction Ignore
rm copasi-dependencies\ -Recurse -Force -ErrorAction Ignore


git clone https://github.com/jonasfoe/copasi-dependencies.git
cd copasi-dependencies\
git checkout ${COPASI_DEPENDENCIES_TAG}
cd ..\

git clone https://github.com/jonasfoe/COPASI.git
cd COPASI\
git checkout ${COPASI_TAG}
cd ..\

@echo off
pushd C:
call "C:\Program Files (x86)\Microsoft Visual C++ Build Tools\vcbuildtools.bat" x86_amd64
popd
%*

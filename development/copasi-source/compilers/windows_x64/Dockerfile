# adapted from https://github.com/StefanScherer/dockerfiles-windows/tree/master/msbuild
FROM microsoft/windowsservercore:10.0.14393.2363

SHELL ["powershell", "-Command", "$ErrorActionPreference = 'Stop'; $ProgressPreference = 'SilentlyContinue';"]

# Installs visualcppbuildtools_full
RUN (New-Object System.Net.WebClient).DownloadFile('http://download.microsoft.com/download/5/f/7/5f7acaeb-8363-451f-9425-68a90f98b238/visualcppbuildtools_full.exe', 'visualcppbuildtools_full.exe') ; \
    Start-Process .\visualcppbuildtools_full.exe -ArgumentList '/NoRestart /S' -Wait ; \
    rm visualcppbuildtools_full.exe

COPY entrypoint.bat entrypoint.bat

# install chocolatey
RUN Set-ExecutionPolicy Bypass -Scope Process -Force; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))

# install cmake and ninja
RUN choco install -y cmake.portable ninja

# install R
#RUN choco install -y R.Project
# or use a snapshot
RUN wget "https://cran.r-project.org/bin/windows/base/R-3.5.0rc-win.exe" -OutFile R-patched-win.exe -UseBasicParsing ; \
    Start-Process -Wait -NoNewWindow -FilePath 'R-patched-win.exe' -ArgumentList '/SILENT','/SUPPRESSMSGBOXES','/SP-','/COMPONENTS="main,x64"','/LOG=C:\\log.txt'

# download swig win 3.0.12
# URLS are unreliable:
# https://sourceforge.net/projects/swig/files/swigwin/swigwin-3.0.12/swigwin-3.0.12.zip
# https://sourceforge.mirrorservice.org/s/sw/swig/swigwin/swigwin-3.0.12/swigwin-3.0.12.zip
RUN wget "https://sourceforge.mirrorservice.org/s/sw/swig/swigwin/swigwin-3.0.12/swigwin-3.0.12.zip" -OutFile swigwin-3.0.12.zip -UseBasicParsing ; \
    Expand-Archive swigwin-3.0.12.zip -DestinationPath . ; \
    rm swigwin-3.0.12.zip ; \
    # rename from version specific to generic
    mv swigwin* swigwin/

# get helper to extract Rlib
COPY r_lib_extractor r_lib_extractor/

RUN cd $env:ProgramFiles/R/R-*/bin/x64/ ; \
    # use the entrypoint for calling the extractor in the correct environment
    &"C:\\entrypoint.bat C:\\r_lib_extractor\expdef64.exe -dRlib.def -l R.dll" ; \
    # dump the Rlib files into C:/Rlib
    cp . C:/Rlib/ -Filter Rlib* -Recurse

SHELL ["cmd.exe", "/s", "/c", "C:\\entrypoint.bat"]

WORKDIR /work

ENTRYPOINT ["C:\\entrypoint.bat"]
CMD ["cmd"]

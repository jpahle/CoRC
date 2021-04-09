# escape=`
# adapted from https://docs.microsoft.com/en-us/visualstudio/install/build-tools-container?view=vs-2019

# Use the latest Windows Server Core image with .NET Framework 4.8.
FROM mcr.microsoft.com/dotnet/framework/sdk:4.8-windowsservercore-ltsc2019

SHELL ["powershell.exe", "-NoLogo", "-ExecutionPolicy", "Bypass", "-Command", "$ErrorActionPreference = 'Stop'; $ProgressPreference = 'SilentlyContinue';"]

# Download the Build Tools bootstrapper.
ADD https://aka.ms/vs/16/release/vs_buildtools.exe C:\TEMP\vs_buildtools.exe

# Install Build Tools excluding workloads and components with known issues.
RUN C:\TEMP\vs_buildtools.exe --quiet --wait --norestart --nocache `
    --installPath C:\BuildTools `
    --includeRecommended `
    --add Microsoft.Component.MSBuild `
    --add Microsoft.VisualStudio.Workload.VCTools

# install chocolatey
RUN iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))

# install cmake and ninja
RUN choco install -y cmake.portable ninja

# install R
RUN choco install -y R.Project
# or use a snapshot
#RUN wget "https://cran.r-project.org/bin/windows/base/R-3.5.0rc-win.exe" -OutFile R-patched-win.exe -UseBasicParsing ; `
#    Start-Process -Wait -NoNewWindow -FilePath 'R-patched-win.exe' -ArgumentList '/SILENT','/SUPPRESSMSGBOXES','/SP-','/COMPONENTS="main,x64"','/LOG=C:\\log.txt'

# download swig win 3.0.12
# URLS are unreliable:
# https://sourceforge.net/projects/swig/files/swigwin/swigwin-3.0.12/swigwin-3.0.12.zip/download
# https://sourceforge.net/projects/swig/files/swigwin/swigwin-3.0.12/swigwin-3.0.12.zip
# https://sourceforge.mirrorservice.org/s/sw/swig/swigwin/swigwin-3.0.12/swigwin-3.0.12.zip
RUN wget "https://sourceforge.net/projects/swig/files/swigwin/swigwin-3.0.12/swigwin-3.0.12.zip/download" -OutFile swigwin-3.0.12.zip -UseBasicParsing -UserAgent "NativeHost" ; `
    Expand-Archive swigwin-3.0.12.zip -DestinationPath . ; `
    rm swigwin-3.0.12.zip ; `
    # rename from version specific to generic
    mv swigwin* swigwin/

# get helper to extract Rlib
COPY r_lib_extractor r_lib_extractor/
 

RUN cd $env:ProgramFiles/R/R-*/bin/x64/ ; `
    # use the entrypoint for calling the extractor in the correct environment
    cmd.exe /c "\"C:\BuildTools\Common7\Tools\VsDevCmd.bat && C:\r_lib_extractor\expdef64.exe -dRlib.def -l R.dll\"" ; `
    # dump the Rlib files into C:/Rlib
    cp . C:/Rlib/ -Filter Rlib* -Recurse

# Restore the default Windows shell for correct batch processing.
SHELL ["cmd", "/S", "/C"]

# Start developer command prompt with any other commands specified.
# ENTRYPOINT C:\BuildTools\Common7\Tools\VsDevCmd.bat -arch=amd64 &&

# Default to PowerShell if no other command specified.
CMD ["powershell.exe", "-NoLogo", "-ExecutionPolicy", "Bypass"]

WORKDIR /work

SET(CMAKE_SYSTEM_NAME Darwin)
# darwinXX is determined by which SDK is installed
SET(TOOLCHAIN_PREFIX x86_64-apple-darwin14)
SET(TOOLCHAIN_SDK 10.10)

# cross compilers to use for C and C++
SET(CMAKE_C_COMPILER o64-clang)
SET(CMAKE_CXX_COMPILER o64-clang++)

# target environment on the build host system
#   set 1st to dir with the cross compiler's C/C++ headers/libs
SET(CMAKE_FIND_ROOT_PATH /opt/osxcross/target/SDK/MacOSX${TOOLCHAIN_SDK}.sdk/usr)
# SET(CMAKE_FIND_ROOT_PATH /opt/osxcross/@TRIPLE@;/opt/osxcross/SDK/MacOSX10.11.sdk/usr)
SET(CMAKE_OSX_SYSROOT /opt/osxcross/target/SDK/MacOSX${TOOLCHAIN_SDK}.sdk/)
SET(CMAKE_INSTALL_FRAMEWORK_PREFIX /opt/osxcross/target/SDK/MacOSX${TOOLCHAIN_SDK}.sdk/System/Library/Frameworks)
SET(CMAKE_AR /opt/osxcross/target/bin/${TOOLCHAIN_PREFIX}-ar CACHE FILEPATH "Archiver")
SET(CMAKE_LINKER /opt/osxcross/target/bin/${TOOLCHAIN_PREFIX}-ld CACHE FILEPATH "Linker")
# SET(CMAKE_EXE_LINKER_FLAGS "-v" CACHE STRING "Flags")
SET(CMAKE_RANLIB /opt/osxcross/target/bin/${TOOLCHAIN_PREFIX}-ranlib)

SET(PKG_CONFIG_EXECUTABLE /opt/osxcross/target/bin/${TOOLCHAIN_PREFIX}-pkg-config)

# adjust the default behaviour of the FIND_XXX() commands:
# search headers and libraries in the target environment, search
# programs in the host environment
SET(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
SET(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
SET(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
SET(CMAKE_FIND_ROOT_PATH_MODE_PACKAGE ONLY)




# this one is commented out as it did not appear to work when set in the script
# however, it does work when given on the command-line
# without it, SFML tries to install some of it's files to '/usr/local/share'
# SET(CMAKE_INSTALL_PREFIX "/home/joshua/Dev_Build/osxcross/target/SDK/MacOSX10.11.sdk/usr/")

# these are required to get the binaries linked correctly, and appears to be a common OSX occurrence 
# SET(CMAKE_SHARED_LIBRARY_RUNTIME_C_FLAG "-Wl,-rpath,")
# SET(CMAKE_SHARED_LIBRARY_RUNTIME_C_FLAG_SEP ":")
# more linking variables
# SET(CMAKE_INSTALL_NAME_DIR "@rpath")
# SET(CMAKE_INSTALL_RPATH ...)

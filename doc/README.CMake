
Building NMOSH with CMake
=========================

LIMITATIONS
-----------

- Performance may _NOT_ optimal when compared with standard build.
- Not every mosh features are supported. (eg. FFI call-back in Win64)
- We recommend standard (autotools) build (ie. ./configure && make) for
  system-installed (ie. /usr/local/bin/mosh) mosh.
- Scheme code tests and some C++ tests are not covered by CTest.
- It does not support psyntax-mosh

0. Supported platform
---------------------

In short, it's Win32/Win64/MacOS X 10.6 or later only. 
Other platforms are may or may not work.

We recommend Ninja generator if available.

- Microsoft Visual Studio 2010 SP1
- Microsoft Visual Studio 2012
- Apple Xcode 4.x and Mac OS X 10.7 (amd64/i386 only)
- GCC/G++ 4.3 or later
- TDM-GCC 4.6 or later

1. Source code setup
--------------------

- (run gen-git-build.sh if you cloned from our git repository.)

setup GMP:
- (Alternative: Setup GStreamer SDK. It includes prebuilt GMP and the 
  buildsystem will automatically detect and link against it.)
- MacOS X: Copy GMP.framework to DYLD default path like ~/Library/Frameworks 
           You can build GMP.framework with misc/dist/mac/build-framework.sh
-    MSVC: Download MPIR 2.x from http://www.mpir.org and build with MSVC
           then set MOSH_GMP_DIR to this
           (c:\XXXXXX\mpir-2.x.x\build.vc10\dll\Win32\Release)
           or
           (c:\XXXXXX\mpir-2.x.x\build.vc10\lib\Win32\Release)

2. Configuration
----------------

TBD.

3. Build
--------

(Just same as standard CMake projects.)

4. Known Issues
---------------

- generic: Big-endian architectures are not supported. Current CMakeList.txt
           assumes target architecture is Little-endian.
- MacOS X: FFI Callback is not supported.
-    MSVC: FFI Callback is not supported at Win64.


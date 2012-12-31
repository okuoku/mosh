#!/bin/sh
mkdir -p build
cd build
export ANDROID_NDK=/Users/oku/Android/android-ndk-r8d
cmake -DNMOSH_WITH_SDL=/Users/oku/repos/SDL -DANDROID_NATIVE_API_LEVEL=android-9 -DANDROID_NO_UNDEFINED=OFF -DNMOSH_GST_SDK_PATH=/Users/oku/Android/gst-sdk-2012.11  -DCMAKE_TOOLCHAIN_FILE=/Users/oku/repos/mosh/misc/dist/android/android.toolchain.cmake /Users/oku/repos/mosh

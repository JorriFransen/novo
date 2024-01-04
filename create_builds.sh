#! /usr/bin/env bash

rm -rf build build-gcc build-release build-gcc-release

# clang builds
CC=clang CXX=clang++ meson setup build
CC=clang CXX=clang++ meson setup --buildtype release build-release

# gcc builds
CC=gcc CXX=g++ meson setup build-gcc
CC=gcc CXX=g++ meson setup --buildtype release build-gcc-release

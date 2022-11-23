#!/usr/bin/env bash
sudo apt-get update

# Remove pre-bundled libunwind
sudo find /usr -name "*libunwind*" -delete

# Use pre-bundled clang
export PATH=/usr/local/clang-5.0.0/bin:$PATH
export CXX=clang++

# Install Boehm GC and libunwind
sudo apt-get install libgc-dev libunwind8-dev

# Build and install re2 from source
git clone https://code.googlesource.com/re2
pushd re2
git checkout 2021-09-01
make -j4 test
sudo make install prefix=/usr
make testinstall prefix=/usr
popd

``FP Test`` is a small C++11 program which tests elementary math
functions in C standard library implementation.

Requirements
------------

- C++11 compatible compiler: [g++](http://gcc.gnu.org/) (version >= 4.8.1), or [clang++](http://clang.llvm.org/cxx_status.html) (version >= 3.3)
- [CMake](http://www.cmake.org)
- [GMP (GNU multiprecision library)](http://gmplib.org/)
- [MPFR (GNU MPFR Library)](http://www.mpfr.org/)

You may install the above requirements on a machine running Ubuntu 12.04 LTS by following the below instructions:

~~~~~~~~~~~~
sudo apt-get install libgmp-dev
sudo apt-get install libmpfr-dev
sudo add-apt-repository ppa:kalakris/cmake -y
sudo apt-get install cmake

sudo add-apt-repository ppa:ubuntu-toolchain-r/test -y
sudo update-alternatives --remove-all gcc
sudo update-alternatives --remove-all g++
sudo apt-get update
sudo apt-get install g++-4.8 -y
sudo apt-get upgrade -y && sudo apt-get dist-upgrade -y

sudo ln -s /usr/bin/g++-4.8 /usr/bin/g++
~~~~~~~~~~~~


Build
-----
Instructions for DEBUG build

    mkdir -p build/debug
    cd build/debug
    cmake -DCMAKE_CXX_COMPILER=<c++-compiler> -DCMAKE_BUILD_TYPE=DEBUG ../../src
    make

Instructions for RELEASE build

    mkdir -p build/release
    cd build/release
    cmake -DCMAKE_CXX_COMPILER=<c++-compiler> -DCMAKE_BUILD_TYPE=RELEASE ../../src
    make

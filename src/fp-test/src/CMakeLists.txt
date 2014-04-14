cmake_minimum_required(VERSION 2.8.7)
project(FP_BUGS_TEST CXX)
set(CMAKE_COLOR_MAKEFILE ON)

# Set Module Path
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake_modules/")

# Initialize CXXFLAGS.
set(CMAKE_CXX_FLAGS                "-Wall -Wextra -std=c++11") # -mfpmath=sse -msse -msse2")
set(CMAKE_CXX_FLAGS_DEBUG          "-g -DDEBUG -DTRACE")
set(CMAKE_CXX_FLAGS_MINSIZEREL     "-Os -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELEASE        "-O3 -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -g")

# MPFR
find_package(MPFR 3.1.0)
include_directories(${MPFR_INCLUDES})
set(EXTRA_LIBS ${EXTRA_LIBS} ${MPFR_LIBRARIES})

# GMP
find_package(GMP 5.0.5)
include_directories(${GMP_INCLUDE_DIR})
set(EXTRA_LIBS ${EXTRA_LIBS} ${GMP_LIBRARIES})

# isnan
include(${CMAKE_MODULE_PATH}/CheckIsNaN.cmake)
if(NOT ISNAN_VARIATION)
  message(STATUS "Could not find working isnan.")
endif(NOT ISNAN_VARIATION)
enable_language(CXX)
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/isnan.h.in
  "\@ISNAN_HEADERS\@\n"
  "#include <type_traits>\n"
  "template<class T>\n"
  "typename std::enable_if<std::is_arithmetic<T>::value, bool> :: type \n"
  "check_nan(T const &_in) { return \@ISNAN_VARIATION\@(_in); }\n"
  )
configure_file(${CMAKE_CURRENT_BINARY_DIR}/isnan.h.in ${CMAKE_CURRENT_BINARY_DIR}/isnan.h)

# isinf
include(${CMAKE_MODULE_PATH}/CheckIsInf.cmake)
if(NOT ISINF_VARIATION)
  message(STATUS "Could not find working isinf.")
endif(NOT ISINF_VARIATION)
enable_language(CXX)
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/isinf.h.in
  "\@ISINF_HEADERS\@\n"
  "#include <type_traits>\n"
  "template<class T>\n"
  "typename std::enable_if<std::is_arithmetic<T>::value, bool> :: type \n"
  "check_inf(T const &_in) { return \@ISINF_VARIATION\@(_in); }\n"
  )
configure_file(${CMAKE_CURRENT_BINARY_DIR}/isinf.h.in ${CMAKE_CURRENT_BINARY_DIR}/isinf.h)

if("${EGLIBCPATH}" MATCHES ".+")
  message(STATUS "Use EGLIBC Installed at ${EGLIBCPATH}")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,--rpath=${EGLIBCPATH}:/usr/lib/x86_64-linux-gnu/:/usr/lib/gcc/x86_64-linux-gnu/4.8 -Wl,--dynamic-linker=${EGLIBCPATH}/lib/ld-linux-x86-64.so.2")
endif()

include_directories("${CMAKE_CURRENT_BINARY_DIR}")
add_executable(fp_bugs_test fp_bugs_test.cpp)
target_link_libraries(fp_bugs_test ${EXTRA_LIBS} -static-libgcc -static-libstdc++)

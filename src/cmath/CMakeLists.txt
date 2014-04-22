cmake_minimum_required(VERSION 2.8.7)
project(CMATH C)
set(CMAKE_COLOR_MAKEFILE ON)

# Initialize CXXFLAGS.
set(CMAKE_C_FLAGS                "-Wall -Wextra -std=c99 -mfpmath=sse -msse -msse2")
set(CMAKE_C_FLAGS_DEBUG          "-g -DDEBUG -DTRACE")
set(CMAKE_C_FLAGS_MINSIZEREL     "-Os -DNDEBUG")
set(CMAKE_C_FLAGS_RELEASE        "-O3 -DNDEBUG")
set(CMAKE_C_FLAGS_RELWITHDEBINFO "-O2 -g")

include_directories("${CMAKE_CURRENT_BINARY_DIR}")
add_executable(sin  branred.c dosincos.c sin.c)
add_executable(ssin simplified.c)
target_link_libraries(sin  ${EXTRA_LIBS})
target_link_libraries(ssin ${EXTRA_LIBS})

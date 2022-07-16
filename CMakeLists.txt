cmake_minimum_required(VERSION 3.0.0)

project(fortran-learning)

enable_language(Fortran C)

set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -coarray=shared -coarray-num-images=2")
add_compile_options(-heap-arrays -logo)
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,-z,stack-size=1000000")

# set(CMAKE_BUILD_TYPE Debug CACHE STRING "set build type to debug")

file(GLOB_RECURSE SRCS src/*.F90)

include_directories(${CMAKE_CURRENT_LIST_DIR}/)
include_directories(${CMAKE_CURRENT_LIST_DIR}/src/)

add_executable(${PROJECT_NAME} ${SRCS})
# set(EXECUTABLE_OUTPUT_PATH ${PROJECTPATH}/run)


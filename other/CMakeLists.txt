# 设置项目所需cmake的最低版本
cmake_minimum_required(VERSION 3.0.0)

# 项目名称，也是最后生成的可执行文件名
project(coarrayforpetsc)

# 启用对Fortran语言的支持
enable_language(Fortran C)

set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -coarray=distributed -coarray-num-images=2")

# find_package(MPI)

# 获取petsc库路径
set(ENV{PKG_CONFIG_PATH} ~/workSpace/programs/petsc/arch-ci-linux-intel-mkl-single/lib/pkgconfig/)
find_package(PkgConfig)
pkg_search_module(PETSCLIB REQUIRED petsc)

include_directories(${PETSCLIB_INCLUDE_DIRS})

# include mpi
include_directories(/opt/intel/oneapi/mpi/2021.3.0/include)

# 设定编译组态，RelWithDebInfo指优化运行速度同时产生debug信息
set(CMAKE_BUILD_TYPE RelWithDebInfo)

#将当前项目里指定目录下的所有文件(包含子目录下的文件)添加到SRCS这个变量里
file(GLOB_RECURSE SRCS RELATIVE ${PROJECT_SOURCE_DIR} *.F90 solve.c)

# 将SRCS编译为mylib这个动态链接库
add_library(mylib SHARED ${SRCS})

# 使用${SRCS}中的源文件生成可执行文件
add_executable(${PROJECT_NAME} ${SRCS})

# 让myprogram依赖mylib
add_dependencies(${PROJECT_NAME} mylib)

# 链接mylib mpilib
target_link_libraries(${PROJECT_NAME} mylib)
target_link_libraries(${PROJECT_NAME} ${PETSCLIB_LIBRARY_DIRS}/libpetsc.so)
target_link_libraries(${PROJECT_NAME} /opt/intel/oneapi/mpi/2021.3.0/lib/libmpifort.so.12.0.0)
target_link_libraries(${PROJECT_NAME} /opt/intel/oneapi/mpi/2021.3.0/lib/release/libmpi.so.12.0.0)

cmake_minimum_required(VERSION 3.0)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules/")

# Create main project.
project(flexout Fortran)

# Use solution folders in IDEs
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

# Specify default build type for single-build-type systems (not VS)
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set (CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

# Customize compiler flags
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-none")
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Cray")
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -N 1023")
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
  if(WIN32)
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} /Od")
  endif()
endif()

# Use use position-independent code (-fPIC) everywhere if building shared libraries
if(BUILD_SHARED_LIBS)
  set(CMAKE_POSITION_INDEPENDENT_CODE ON)
endif()

# Make sure all module files (*.mod) are written to one single "modules" directory.
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)

# Global include directories
include_directories(BEFORE "${CMAKE_CURRENT_BINARY_DIR}/modules")

if (NOT TARGET yaml)
  if (NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/../lib/fortran-yaml/CMakeLists.txt")
    find_package(Git QUIET)
    if(GIT_FOUND AND EXISTS "${PROJECT_SOURCE_DIR}/../.git")
      execute_process(COMMAND ${GIT_EXECUTABLE} submodule update --init WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
    endif()
  endif()
  if (NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/../lib/fortran-yaml/CMakeLists.txt")
    message(FATAL_ERROR "Fortran-YAML source code not found. Initialize git submodules first (git submodule update --init)")
  endif()
  add_subdirectory("${CMAKE_CURRENT_SOURCE_DIR}/../lib/fortran-yaml" "${CMAKE_CURRENT_BINARY_DIR}/yaml")
endif()

add_library(output_manager
            field_manager.F90
            output_manager.F90
            output_manager_core.F90
            formats/netcdf_output.F90
            formats/text_output.F90
            operators/library.F90
            operators/base.F90
            operators/time_average.F90
            operators/interp.F90
            operators/slice.F90
            )
source_group(operators REGULAR_EXPRESSION operators)
source_group(formats REGULAR_EXPRESSION formats)
target_compile_definitions(output_manager PRIVATE _ONE_=1.d0)
target_link_libraries(output_manager PRIVATE yaml)

option(FLEXOUT_USE_NetCDF "Enable output in NetCDF format" ON)
if(FLEXOUT_USE_NetCDF)
  find_package(NetCDF REQUIRED)
  target_include_directories(output_manager PRIVATE "${NetCDF_INCLUDE_DIRS}")
  target_link_libraries(output_manager PUBLIC "${NetCDF_LIBRARIES}")
  target_compile_definitions(output_manager PRIVATE -DNETCDF_FMT)
  if(NetCDF_STATIC_MSVC_BUILD)
    target_compile_options(output_manager PUBLIC "/libs:static")
    target_link_libraries(output_manager PUBLIC debug "/NODEFAULTLIB:\"libcmt\"")
  endif()
endif(FLEXOUT_USE_NetCDF)


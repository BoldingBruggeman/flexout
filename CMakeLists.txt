cmake_minimum_required(VERSION 3.10)

# Create main project.
project(flexout VERSION 0.1.0 LANGUAGES Fortran)

# Use solution folders in IDEs
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

# Specify default build type for single-build-type systems (not VS)
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set (CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules/")

# Better set them for each target using target_compile_{definitions,features,options}
# Customize compiler flags
#[[---
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-none")
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Cray")
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -N 1023")
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
  if(WIN32)
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} /Od")
  endif()
endif()
---]]

# Use use position-independent code (-fPIC) everywhere if building shared libraries
if(BUILD_SHARED_LIBS)
  set(CMAKE_POSITION_INDEPENDENT_CODE ON)
endif()

option(FLEXOUT_USE_NetCDF "Enable output in NetCDF format" ON)

if(FLEXOUT_USE_NetCDF)
  find_package(NetCDF REQUIRED)
endif()

if (NOT TARGET yaml)
  if (NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/extern/fortran-yaml/CMakeLists.txt")
    message(FATAL_ERROR "Fortran-YAML not found at extern/fortran-yaml. Please retrieve this submodule first by running \"git submodule update --init\" within your flexout source directory.")
  endif()
  add_subdirectory("${CMAKE_CURRENT_SOURCE_DIR}/extern/fortran-yaml" "${CMAKE_CURRENT_BINARY_DIR}/extern/yaml")
  set_property(TARGET yaml PROPERTY FOLDER flexout)
endif()

add_subdirectory(src)
add_subdirectory(test)

install(EXPORT flexoutConfig DESTINATION cmake)


cmake_minimum_required(VERSION 3.0)

project(output_manager VERSION 0.1.0 LANGUAGES Fortran)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules/")

if (NOT TARGET yaml)
  if (NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/../extern/fortran-yaml/CMakeLists.txt")
    find_package(Git QUIET)
    if(GIT_FOUND AND EXISTS "${PROJECT_SOURCE_DIR}/../.git")
      execute_process(COMMAND ${GIT_EXECUTABLE} submodule update --init WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
    endif()
  endif()
  if (NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/../extern/fortran-yaml/CMakeLists.txt")
    message(FATAL_ERROR "Fortran-YAML source code not found. Initialize git submodules first (git submodule update --init)")
  endif()
  add_subdirectory("${CMAKE_CURRENT_SOURCE_DIR}/../extern/fortran-yaml" "${CMAKE_CURRENT_BINARY_DIR}/extern/yaml")
  set_property(TARGET yaml PROPERTY FOLDER flexout)
endif()

if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
  add_compile_options("-ffree-line-length-none")
endif()

add_definitions(-D_ONE_=1.d0)

add_library(field_manager
    field_manager.F90
)

add_library(output_manager
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
set_property(TARGET field_manager output_manager PROPERTY Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)
target_include_directories(output_manager
    PUBLIC
        $<INSTALL_INTERFACE:include>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/modules>
)
target_include_directories(field_manager
    PUBLIC
        $<INSTALL_INTERFACE:include>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/modules>
)
target_link_libraries(output_manager PRIVATE field_manager yaml)

set_property(TARGET output_manager field_manager PROPERTY FOLDER flexout)

option(FLEXOUT_USE_NetCDF "Enable output in NetCDF format" ON)
if(FLEXOUT_USE_NetCDF)
  target_link_libraries(output_manager PUBLIC netcdf)
  target_compile_definitions(output_manager PRIVATE -DNETCDF_FMT)
endif(FLEXOUT_USE_NetCDF)

install(TARGETS field_manager output_manager 
        EXPORT output_manager-targets
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
)

install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules/
        DESTINATION include)

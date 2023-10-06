set(GOTM_MSVC_NetCDF_DIR "${CMAKE_CURRENT_LIST_DIR}/../../../../extras/netcdf")
set(AUTODETECTED_NetCDF OFF)

if (MSVC AND EXISTS "${GOTM_MSVC_NetCDF_DIR}")

  # On Windows using MS Visual Studio, with GOTM-supplied NetCDF available:
  # default to using that.
  if(CMAKE_SIZEOF_VOID_P EQUAL 8)
    set(DEFAULT_NetCDF_ROOT "${GOTM_MSVC_NetCDF_DIR}/Win64/3.6.3")
  else()
    set(DEFAULT_NetCDF_ROOT "${GOTM_MSVC_NetCDF_DIR}/Win32/3.6.3")
  endif()
  set(DEFAULT_INCLUDE_DIR ${DEFAULT_NetCDF_ROOT}/include)
  set(DEFAULT_LIBRARY_DIR ${DEFAULT_NetCDF_ROOT}/lib)
  set(DEFAULT_LIBRARY_NAME netcdfs)
  get_filename_component(GOTM_MSVC_NetCDF_LIBRARY "${DEFAULT_NetCDF_ROOT}/lib/netcdfs.lib" ABSOLUTE)
  set(AUTODETECTED_NetCDF ON)

else()

  # First try to locate nf-config.
  find_program(NetCDF_CONFIG_EXECUTABLE
    NAMES nf-config
    HINTS ENV NetCDF_ROOT
    PATH_SUFFIXES bin Bin
    DOC "NetCDF config program. Used to detect NetCDF include directory and linker flags." )
  mark_as_advanced(NetCDF_CONFIG_EXECUTABLE)

  if(NetCDF_CONFIG_EXECUTABLE)

    # Found nf-config - use it to retrieve include directory and linking flags.
    # Note that if the process fails (as e.g. on Windows), the output variables
    # will remain empty
    execute_process(COMMAND ${NetCDF_CONFIG_EXECUTABLE} --includedir
                    OUTPUT_VARIABLE DEFAULT_INCLUDE_DIR
                    OUTPUT_STRIP_TRAILING_WHITESPACE)
    execute_process(COMMAND ${NetCDF_CONFIG_EXECUTABLE} --flibs
                    OUTPUT_VARIABLE flibs
                    OUTPUT_STRIP_TRAILING_WHITESPACE)
    if (flibs)
      set(NetCDF_LIBRARIES ${flibs} CACHE STRING "NetCDF libraries (or linking flags)")
      set(AUTODETECTED_NetCDF ON)
    endif()

  endif()

  # Determine default name of NetCDf library
  # If nf-config succeeded, its result takes priority as it has already been
  # used to set NetCDF_LIBRARIES
  if(DEFINED ENV{NetCDFLIBNAME})
    set(DEFAULT_LIBRARY_NAME "$ENV{NETCDFLIBNAME}")
  else()
    set(DEFAULT_LIBRARY_NAME netcdff)
  endif()

endif()

find_path(NetCDF_INCLUDE_DIRS netcdf.mod
  HINTS "${DEFAULT_INCLUDE_DIR}" "$ENV{NETCDFINC}" "$ENV{CONDA_PREFIX}/Library/include"
  DOC "NetCDF include directories")

find_library(NetCDF_LIBRARIES NAMES ${DEFAULT_LIBRARY_NAME}
            HINTS "${DEFAULT_LIBRARY_DIR}" "$ENV{NETCDFLIBDIR}" "$ENV{CONDA_PREFIX}/Library/lib"
            DOC "NetCDF libraries (or linking flags)")

if(AUTODETECTED_NetCDF)
  mark_as_advanced(NetCDF_INCLUDE_DIRS NetCDF_LIBRARIES)
endif()

if(GOTM_MSVC_NetCDF_LIBRARY)
  # On Windows with MS Visual Studio, with GOTM-supplied NetCDF available
  # Check if we are using that
  list(LENGTH NetCDF_LIBRARIES LIBCOUNT)
  if(LIBCOUNT EQUAL 1)
    # GOTM-supplied NetCDF libraries are statically built against release version of runtime libraries.
    # If one of those was used, dependent projects need to do the same in release mode to prevent linking conflicts.
    get_filename_component(NetCDF_LIBRARY ${NetCDF_LIBRARIES} ABSOLUTE)
    string(COMPARE EQUAL "${NetCDF_LIBRARY}" "${GOTM_MSVC_NetCDF_LIBRARY}" STAT)
    option(NetCDF_STATIC_MSVC_BUILD "NetCDF library is statically linked to runtime libraries" ${STAT})
    mark_as_advanced(NetCDF_STATIC_MSVC_BUILD)
  endif()
endif()

# Process default arguments (QUIET, REQUIRED)
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args (NetCDF DEFAULT_MSG NetCDF_LIBRARIES NetCDF_INCLUDE_DIRS)

add_library(netcdf INTERFACE IMPORTED GLOBAL)
set_property(TARGET netcdf APPEND PROPERTY INTERFACE_INCLUDE_DIRECTORIES "${NetCDF_INCLUDE_DIRS}")
set_property(TARGET netcdf APPEND PROPERTY INTERFACE_LINK_LIBRARIES "${NetCDF_LIBRARIES}")
if(NetCDF_STATIC_MSVC_BUILD)
  message("Using statically built NetCDF libraries in combination with Visual Studio. Forcing all projects to link statically against runtime.")
  set_property(DIRECTORY ${CMAKE_SOURCE_DIR} APPEND PROPERTY COMPILE_OPTIONS /libs:static)
  set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} APPEND PROPERTY COMPILE_OPTIONS /libs:static)
  set_property(TARGET netcdf APPEND PROPERTY INTERFACE_LINK_LIBRARIES $<$<CONFIG:DEBUG>:-NODEFAULTLIB:libcmt>)
endif()

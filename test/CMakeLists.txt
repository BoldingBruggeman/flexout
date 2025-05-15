cmake_minimum_required(VERSION 3.10)

add_executable(test_flexout EXCLUDE_FROM_ALL
               test_flexout.F90
              )
target_link_libraries(test_flexout PRIVATE output_manager yaml)

set_property(TARGET test_flexout PROPERTY FOLDER flexout)

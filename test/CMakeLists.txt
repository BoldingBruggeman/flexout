cmake_minimum_required(VERSION 3.0)

add_executable(test_flexout
               test_flexout.F90
              )
target_link_libraries(test_flexout PRIVATE output_manager yaml)

install(TARGETS test_flexout
        RUNTIME DESTINATION bin
)

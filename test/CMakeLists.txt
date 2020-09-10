################################################################################
# Copy test code and data to the build area

add_custom_target(copy_test_data ALL ${CMAKE_COMMAND} -E copy_directory
  ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_BINARY_DIR}/test)

################################################################################
# macro to link the musica-photo-utils library and add the test to the suite

macro(add_unit_test test_name)
  target_include_directories(${test_name} PUBLIC ${CMAKE_BINARY_DIR}/src)
  target_link_libraries(${test_name} musica-photo-utils-lib)
  add_test(NAME ${test_name} COMMAND ${test_name})
endmacro(add_unit_test)

################################################################################
# Add all the tests to the test suite

add_executable(numer_test numer/test.F90)
add_unit_test(numer_test)

################################################################################

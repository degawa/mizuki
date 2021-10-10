set(EXTERNAL_LIB_LINKER_FLAGS "")
if (${CMAKE_GENERATOR} MATCHES "Visual Studio*")
    list(APPEND EXTERNAL_LIB_LINKER_FLAGS " /INCREMENTAL:NO /NOLOGO /NODEFAULTLIB:\"libifcoremt.lib\" /NODEFAULTLIB:\"LIBCMT\"")
    # see community.intel.com/t5/Intel-Fortran-Compiler/Problem-linking-external-lib-in-VS2017-and-IPSXE20/m-p/1140017#M136862
endif()

set(TEST_MOD_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/mod/${COMPILER_ID})
message("test module output dir: ${CMAKE_CURRENT_BINARY_DIR}/mod/${COMPILER_ID}")
message("")


# テストを登録するマクロ
function(ADDTEST_LIB name)
    add_executable(test_${name} test_${name}.f90)

    set_target_properties(test_${name}
        PROPERTIES
        Fortran_MODULE_DIRECTORY ${INCLUDE_DIR}
        Fortran_MODULE_DIRECTORY ${TEST_MOD_OUTPUT_DIR}
        LINK_FLAGS "${EXTERNAL_LIB_LINKER_FLAGS}"
    )
    target_include_directories(test_${name}
        PUBLIC ${INCLUDE_DIR}
    )
    target_link_libraries(test_${name}
        PRIVATE
        stdlib
    )

    add_test(NAME ${name}
             COMMAND $<TARGET_FILE:test_${name}> ${CMAKE_CURRENT_BINARY_DIR}
             WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
            )
endfunction(ADDTEST_LIB)

add_subdirectory(FortranStdlib)

set(EXTERNAL_LIB_LINKER_FLAGS "")
if (${CMAKE_GENERATOR} MATCHES "Visual Studio*")
    list(APPEND EXTERNAL_LIB_LINKER_FLAGS " /INCREMENTAL:NO /NOLOGO /NODEFAULTLIB:\"libifcoremt.lib\" /NODEFAULTLIB:\"LIBCMT\"")
    # see community.intel.com/t5/Intel-Fortran-Compiler/Problem-linking-external-lib-in-VS2017-and-IPSXE20/m-p/1140017#M136862
endif()

set(TEST_MOD_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/mod/${COMPILER_ID})
message("test module output dir: ${CMAKE_CURRENT_BINARY_DIR}/mod/${COMPILER_ID}")
message("")

# ビルドしたMIZUKIライブラリの読み込み設定
## ライブラリ名の設定
if(CMAKE_Fortran_COMPILER_ID MATCHES GNU OR Fortran_COMPILER_NAME MATCHES "gfortran*")
    set(MIZUKI_LIBNAME libmizuki.a)
elseif(CMAKE_Fortran_COMPILER_ID MATCHES Intel OR Fortran_COMPILER_NAME MATCHES "ifort*")
    set(MIZUKI_LIBNAME mizuki.lib)
endif()

## ライブラリの読み込み
add_library(mizuki_lib STATIC IMPORTED GLOBAL)
set_target_properties(mizuki_lib
    PROPERTIES
    IMPORTED_LOCATION ${LIB_OUTPUT_DIR}/${MIZUKI_LIBNAME}
)

# assertモジュールの追加
add_library(assert STATIC)
target_sources(assert
    PRIVATE
    assertion/src/assertion.f90
    assertion/src/common/checkTrue.f90
    assertion/src/common/optval.f90
    assertion/src/common/setTolerance.f90
    assertion/src/common/status.f90
    assertion/src/common/stopOnFailure.f90
    assertion/src/equal/assertEqual.f90
    assertion/src/equal/expectEqual.f90
    assertion/src/equal/compareArrayValues.f90
    assertion/src/equal/outputOnFailure.f90
    assertion/src/sameShape/assertSameShape.f90
    assertion/src/sameShape/expectSameShape.f90
    assertion/src/sameShape/compareArrayShape.f90
    assertion/src/logical/expectLogical.f90
    assertion/src/logical/assertLogical.f90
)

set_target_properties(assert
    PROPERTIES
    Fortran_MODULE_DIRECTORY ${TEST_MOD_OUTPUT_DIR}
)

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
        PUBLIC
        ${INCLUDE_DIR}
        ${MOD_OUTPUT_DIR}
    )
    target_link_libraries(test_${name}
        PRIVATE
        mizuki_lib
        assert
        stdlib
    )

    add_test(NAME ${name}
             COMMAND $<TARGET_FILE:test_${name}> ${CMAKE_CURRENT_BINARY_DIR}
             WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
            )
endfunction(ADDTEST_LIB)

add_subdirectory(assertion)
add_subdirectory(FortranStdlib)
add_subdirectory(axis)
add_subdirectory(Cartesian)
add_subdirectory(arrayRange)

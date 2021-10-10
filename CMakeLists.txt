# CMakeのバージョン
cmake_minimum_required(VERSION 3.15)

# Fortran向け設定の有効化
enable_language(Fortran)

# プロジェクト名と使用する言語
project(mizuki Fortran)

# テストの有効化
enable_testing()

# コンパイルオプション
if(CMAKE_Fortran_COMPILER_ID MATCHES GNU OR Fortran_COMPILER_NAME MATCHES "gfortran*")
    set(CMAKE_Fortran_FLAGS         "-ffree-line-length-none -fimplicit-none -std=f2018 -cpp")
    set(CMAKE_Fortran_FLAGS_DEBUG   "-Wall -Wno-maybe-uninitialized -fbacktrace -fcheck=all,no-array-temps -O0 -g")
    set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -march=native")
    set(COMPILER_ID gnu)
    set(Fortran_COMPILER_NAME gfortran)
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES Intel OR Fortran_COMPILER_NAME MATCHES "ifort*")
    if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 19.0)
        set(STD_FLAG_KEYWORD "f15")
    else()
        set(STD_FLAG_KEYWORD "f18")
    endif()

    if(CMAKE_SYSTEM_NAME STREQUAL "Windows")
        set(CMAKE_Fortran_FLAGS         "/stand:${STD_FLAG_KEYWORD} /fpp /nologo")
        set(CMAKE_Fortran_FLAGS_DEBUG   "/warn:all /Qdiag-disable:5462 /Qdiag-disable:5194 /traceback /check:all /check:noarg_temp_created /debug:full /Od")
        set(CMAKE_Fortran_FLAGS_RELEASE "/O3 /QxHost")
    else()
        set(CMAKE_Fortran_FLAGS         "-stand ${STD_FLAG_KEYWORD} -fpp -nologo")
        set(CMAKE_Fortran_FLAGS_DEBUG   "-warn all -traceback -check all,noarg_temp_created -debug full -O0")
        set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -xHost")
    endif()
    set(COMPILER_ID intel)
    set(Fortran_COMPILER_NAME ifort)
endif()

# ディレクトリ設定
set(INCLUDE_DIR ${CMAKE_SOURCE_DIR}/include/${COMPILER_ID})
set(LIB_DIR ${CMAKE_SOURCE_DIR}/lib/${COMPILER_ID})

set(MOD_OUTPUT_DIR ${PROJECT_BINARY_DIR}/mod/${COMPILER_ID})
set(LIB_OUTPUT_DIR ${PROJECT_BINARY_DIR}/lib/${COMPILER_ID})
set(BIN_OUTPUT_DIR ${PROJECT_BINARY_DIR}/bin)

message("include dir: ${INCLUDE_DIR}")
message("library dir: ${LIB_DIR}")
message("")

message("module  file output dir: ${MOD_OUTPUT_DIR}")
message("library file output dir: ${LIB_OUTPUT_DIR}")
message("binary  file output dir: ${BIN_OUTPUT_DIR}")
message("")

########################################################################
#外部ライブラリの設定
if(CMAKE_Fortran_COMPILER_ID MATCHES GNU OR Fortran_COMPILER_NAME MATCHES "gfortran*")
    set(STDLIB_LIBNAME libfortran_stdlib.a)
elseif(CMAKE_Fortran_COMPILER_ID MATCHES Intel OR Fortran_COMPILER_NAME MATCHES "ifort*")
    set(STDLIB_LIBNAME fortran_stdlib.lib)
endif()

# 外部プロクジェクト読み込み準備
include(ExternalProject)

# fortran-stdlibのダウンロードとビルド
if(NOT EXISTS "${LIB_DIR}/${STDLIB_LIBNAME}")
    message("add external project: fortran-stdlib (external/stdlib)")

    # maximum_rankが指定されていない場合はmaximum_rankを7とする
    if(NOT DEFINED STDLIB_MAXIMUM_RANK)
        set(STDLIB_MAXIMUM_RANK 7)
    endif()

    # ビルド設定
    set(PRJ_STDLIB_CMAKE_ARGS)
    list(APPEND PRJ_STDLIB_CMAKE_ARGS -DCMAKE_Fortran_COMPILER=${Fortran_COMPILER_NAME})
    list(APPEND PRJ_STDLIB_CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${CMAKE_SOURCE_DIR})
    list(APPEND PRJ_STDLIB_CMAKE_ARGS -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE})
    list(APPEND PRJ_STDLIB_CMAKE_ARGS -Dfortran_stdlib_ENABLE_INSTALL_SUBDIR=${LIBRARY_ENABLE_INSTALL_SUBDIR})
    list(APPEND PRJ_STDLIB_CMAKE_ARGS -DCMAKE_MAXIMUM_RANK=${STDLIB_MAXIMUM_RANK})
    list(APPEND PRJ_STDLIB_CMAKE_ARGS -DBUILD_SHARED_LIBS=OFF)
    if (${CMAKE_GENERATOR} MATCHES "Visual Studio*")
        list(APPEND PRJ_STDLIB_CMAKE_ARGS -DCMAKE_GENERATOR_PLATFORM=${CMAKE_GENERATOR_PLATFORM})
    endif()

    ExternalProject_Add(
        fortran-stdlib # target name. add_libraryのライブラリ名と衝突するので，ファイル名（.aやlib）とは異なる名前を付ける

        GIT_REPOSITORY https://github.com/degawa/stdlib
        GIT_TAG origin/master

        SOURCE_DIR ${CMAKE_SOURCE_DIR}/external/stdlib
        BINARY_DIR ${CMAKE_SOURCE_DIR}/external/stdlib # ビルドディレクトリではなく作業ディレクトリを指定する

        CMAKE_GENERATOR ${CMAKE_GENERATOR}
        INSTALL_DIR ${CMAKE_SOURCE_DIR}
        CMAKE_ARGS
            ${PRJ_STDLIB_CMAKE_ARGS}

        CONFIGURE_COMMAND cmake -B <BINARY_DIR>/build -G ${CMAKE_GENERATOR} ${PRJ_STDLIB_CMAKE_ARGS}
        BUILD_COMMAND cmake --build <BINARY_DIR>/build --config $<CONFIG>
        INSTALL_COMMAND cmake --install <BINARY_DIR>/build --config $<CONFIG> --prefix ${CMAKE_SOURCE_DIR}
    )

endif()
########################################################################

add_subdirectory(src)
add_subdirectory(tests)

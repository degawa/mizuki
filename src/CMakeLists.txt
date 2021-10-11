# 出力ファイル名
set(LIBNAME ${PROJECT_NAME})

## 既存の（あるいはビルド＆コピーした）fortran-stdlibライブラリをプロジェクトに追加
add_library(stdlib STATIC IMPORTED GLOBAL)
set_target_properties(stdlib
    PROPERTIES
    IMPORTED_LOCATION ${LIB_DIR}/${STDLIB_LIBNAME}
)

# MIZUKIライブラリ用ソースファイル
FILE(GLOB_RECURSE BASE_MODULE_SRC "base/*.f90")

add_library(${LIBNAME} STATIC)
target_sources(${LIBNAME}
    PUBLIC
    ${BASE_MODULE_SRC}
)

target_link_libraries(${LIBNAME}
    PRIVATE
    stdlib
)

# ライブラリの出力ディレクトリ設定
set_target_properties(${LIBNAME}
    PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${LIB_OUTPUT_DIR}
    LIBRARY_OUTPUT_DIRECTORY ${LIB_OUTPUT_DIR}
    Fortran_MODULE_DIRECTORY ${MOD_OUTPUT_DIR}
)

target_include_directories(${LIBNAME}
    PUBLIC
    ${INCLUDE_DIR}
)

# 実行ファイルのビルド設定
set(BINNAME "${PROJECT_NAME}_")

add_executable(${BINNAME}
    main.f90
)

target_include_directories(${BINNAME}
    PRIVATE
    ${INCLUDE_DIR}
    ${MOD_OUTPUT_DIR}
)
target_link_libraries(${BINNAME}
    PRIVATE
    ${LIBNAME}
    stdlib
)
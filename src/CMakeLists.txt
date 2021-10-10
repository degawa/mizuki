# 出力ファイル名
set(BINNAME ${PROJECT_NAME})

## 既存の（あるいはビルド＆コピーした）fortran-stdlibライブラリをプロジェクトに追加
add_library(stdlib STATIC IMPORTED GLOBAL)
set_target_properties(stdlib
    PROPERTIES
    IMPORTED_LOCATION ${LIB_DIR}/${STDLIB_LIBNAME}
)

# TopCAST-extライブラリ用ソースファイル
FILE(GLOB_RECURSE BASE_MODULE_SRC "base/*.f90")

add_executable(${BINNAME})
target_sources(${BINNAME}
    PUBLIC
    main.f90
    ${BASE_MODULE_SRC}
)

target_link_libraries(${BINNAME}
    PRIVATE
    stdlib
)

# 実行ファイルのプロパティ
set_target_properties(${BINNAME}
    PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${LIB_OUTPUT_DIR}
    LIBRARY_OUTPUT_DIRECTORY ${LIB_OUTPUT_DIR}
    Fortran_MODULE_DIRECTORY ${MOD_OUTPUT_DIR}
)

target_include_directories(${BINNAME}
    PUBLIC
    ${INCLUDE_DIR}
)

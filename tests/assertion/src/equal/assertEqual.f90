!| 変数あるいは配列の値が等しいかを検査する手続を定義したモジュール．
module assertion_equal_assertEqual
    use, intrinsic :: iso_fortran_env
    use :: assertion_equal_expect
    implicit none
    private
    public :: assert_equal

    interface assert_equal
        procedure :: assert_equal_int8_int8
        procedure :: assert_equal_int16_int16
        procedure :: assert_equal_int32_int32
        procedure :: assert_equal_int64_int64
        procedure :: assert_equal_1dInt32_1dInt32
        procedure :: assert_equal_2dInt32_2dInt32
        procedure :: assert_equal_3dInt32_3dInt32
        procedure :: assert_approxequal_real32_real32
        procedure :: assert_approxequal_real64_real64
        procedure :: assert_approxequal_1dReal32_1dReal32
        procedure :: assert_approxequal_2dReal32_2dReal32
        procedure :: assert_approxequal_3dReal32_3dReal32
        procedure :: assert_approxequal_1dReal64_1dReal64
        procedure :: assert_approxequal_2dReal64_2dReal64
        procedure :: assert_approxequal_3dReal64_3dReal64
        procedure :: assert_equiv_logical_logical
        procedure :: assert_equal_str_str
        procedure :: assert_equal_charArray_charArray
    end interface

contains

    !| 1バイト整数同士の値が等しいかを検査する<br>
    ! 同じであれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する．<br>
    ! テストが失敗したときか，`verbose`が`.true.`に
    ! 設定されている場合に，詳細を表示する
    subroutine assert_equal_int8_int8(actual, expected, test_name, verbose)
        implicit none
        integer(int8), intent(in) :: actual
        integer(int8), intent(in) :: expected
        character(*), intent(in) :: test_name
        logical, intent(in), optional :: verbose

        logical :: test_stat

        call expect_equal(actual, expected, test_name, test_stat, verbose)
        call stop_on_failure(test_stat)
    end subroutine assert_equal_int8_int8

    !| 2バイト整数同士の値が等しいかを検査する<br>
    ! 同じであれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する．<br>
    ! テストが失敗したときか，`verbose`が`.true.`に
    ! 設定されている場合に，詳細を表示する
    subroutine assert_equal_int16_int16(actual, expected, test_name, verbose)
        implicit none
        integer(int16), intent(in) :: actual
        integer(int16), intent(in) :: expected
        character(*), intent(in) :: test_name
        logical, intent(in), optional :: verbose

        logical :: test_stat

        call expect_equal(actual, expected, test_name, test_stat, verbose)
        call stop_on_failure(test_stat)
    end subroutine assert_equal_int16_int16

    !| 1バイト整数同士の値が等しいかを検査する<br>
    ! 同じであれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する．<br>
    ! テストが失敗したときか，`verbose`が`.true.`に
    ! 設定されている場合に，詳細を表示する
    subroutine assert_equal_int32_int32(actual, expected, test_name, verbose)
        implicit none
        integer(int32), intent(in) :: actual
        integer(int32), intent(in) :: expected
        character(*), intent(in) :: test_name
        logical, intent(in), optional :: verbose

        logical :: test_stat

        call expect_equal(actual, expected, test_name, test_stat, verbose)
        call stop_on_failure(test_stat)
    end subroutine assert_equal_int32_int32

    !| 8バイト整数同士の値が等しいかを検査する<br>
    ! 同じであれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する．<br>
    ! テストが失敗したときか，`verbose`が`.true.`に
    ! 設定されている場合に，詳細を表示する
    subroutine assert_equal_int64_int64(actual, expected, test_name, verbose)
        implicit none
        integer(int64), intent(in) :: actual
        integer(int64), intent(in) :: expected
        character(*), intent(in) :: test_name
        logical, intent(in), optional :: verbose

        logical :: test_stat

        call expect_equal(actual, expected, test_name, test_stat, verbose)
        call stop_on_failure(test_stat)
    end subroutine assert_equal_int64_int64

    !| ランク1の4バイト整数型配列同士の値が等しいかを検査する<br>
    ! 同じであれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する．
    subroutine assert_equal_1dInt32_1dInt32(actual, expected, test_name)
        use :: assertion_sameShape_expect
        use :: assert_equal_compareArrayValues
        implicit none

        integer(int32), intent(in) :: actual(:)
        integer(int32), intent(in) :: expected(:)
        character(*), intent(in) :: test_name

        logical :: test_stat

        call expect_equal(actual, expected, test_name, test_stat)
        call stop_on_failure(test_stat)
    end subroutine assert_equal_1dInt32_1dInt32

    !| ランク2の4バイト整数型配列同士の値が等しいかを検査する<br>
    ! 同じであれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する．
    subroutine assert_equal_2dInt32_2dInt32(actual, expected, test_name)
        use :: assertion_sameShape_expect
        use :: assert_equal_compareArrayValues
        implicit none

        integer(int32), intent(in) :: actual(:, :)
        integer(int32), intent(in) :: expected(:, :)
        character(*), intent(in) :: test_name

        logical :: test_stat

        call expect_equal(actual, expected, test_name, test_stat)
        call stop_on_failure(test_stat)
    end subroutine assert_equal_2dInt32_2dInt32

    !| ランク3の4バイト整数型配列同士の値が等しいかを検査する<br>
    ! 同じであれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する．
    subroutine assert_equal_3dInt32_3dInt32(actual, expected, test_name)
        use :: assertion_sameShape_expect
        use :: assert_equal_compareArrayValues
        implicit none

        integer(int32), intent(in) :: actual(:, :, :)
        integer(int32), intent(in) :: expected(:, :, :)
        character(*), intent(in) :: test_name

        logical :: test_stat

        call expect_equal(actual, expected, test_name, test_stat)
        call stop_on_failure(test_stat)
    end subroutine assert_equal_3dInt32_3dInt32

    !| 単精度実数同士の値が等しいかを検査する<br>
    ! 許容誤差は`tolerance`で指定し，指定されない時は
    ! マシンイプシロンを使用する．<br>
    ! 指定された誤差の範囲内で同じであれば，
    ! `PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する．<br>
    ! テストが失敗したときか，`verbose`が`.true.`に
    ! 設定されている場合に，詳細を表示する
    subroutine assert_approxequal_real32_real32(actual, expected, test_name, tolerance, verbose)
        use :: assertion_common_setTolerance
        implicit none

        real(real32), intent(in) :: actual
        real(real32), intent(in) :: expected
        character(*), intent(in) :: test_name
        real(real32), intent(in), optional :: tolerance
        logical, intent(in), optional :: verbose

        logical :: test_stat

        call expect_equal(actual, expected, test_name, test_stat, tolerance, verbose)
        call stop_on_failure(test_stat)
    end subroutine assert_approxequal_real32_real32

    !| 倍精度実数同士の値が等しいかを検査する<br>
    ! 許容誤差は`tolerance`で指定し，指定されない時は
    ! マシンイプシロンを使用する．<br>
    ! 指定された誤差の範囲内で同じであれば，
    ! `PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する．<br>
    ! テストが失敗したときか，`verbose`が`.true.`に
    ! 設定されている場合に，詳細を表示する
    subroutine assert_approxequal_real64_real64(actual, expected, test_name, tolerance, verbose)
        use :: assertion_common_setTolerance
        implicit none

        real(real64), intent(in) :: actual
        real(real64), intent(in) :: expected
        character(*), intent(in) :: test_name
        real(real64), intent(in), optional :: tolerance
        logical, intent(in), optional :: verbose

        logical :: test_stat

        call expect_equal(actual, expected, test_name, test_stat, tolerance, verbose)
        call stop_on_failure(test_stat)
    end subroutine assert_approxequal_real64_real64

    !| ランク1の単精度実数配列同士の値が等しいかを検査する<br>
    ! 許容誤差は`tolerance`で指定し，指定されない時は
    ! マシンイプシロンを使用する．<br>
    ! 指定された誤差の範囲内で同じであれば，
    ! `PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する．<br>
    ! テストが失敗したときか，`verbose`が`.true.`に
    ! 設定されている場合に，詳細を表示する
    subroutine assert_approxequal_1dReal32_1dReal32(actual, expected, test_name, tolerance, verbose)
        use :: assertion_sameShape_expect
        use :: assert_equal_compareArrayValues
        use :: assertion_common_setTolerance
        implicit none

        real(real32), intent(in) :: actual(:)
        real(real32), intent(in) :: expected(:)
        character(*), intent(in) :: test_name
        real(real32), intent(in), optional :: tolerance
        logical, intent(in), optional :: verbose

        logical :: test_stat

        call expect_equal(actual, expected, test_name, test_stat, tolerance, verbose)
        call stop_on_failure(test_stat)
    end subroutine assert_approxequal_1dReal32_1dReal32

    !| ランク2の単精度実数配列同士の値が等しいかを検査する<br>
    ! 許容誤差は`tolerance`で指定し，指定されない時は
    ! マシンイプシロンを使用する．<br>
    ! 指定された誤差の範囲内で同じであれば，
    ! `PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する．<br>
    ! テストが失敗したときか，`verbose`が`.true.`に
    ! 設定されている場合に，詳細を表示する
    subroutine assert_approxequal_2dReal32_2dReal32(actual, expected, test_name, tolerance, verbose)
        use :: assertion_sameShape_expect
        use :: assert_equal_compareArrayValues
        use :: assertion_common_setTolerance
        implicit none

        real(real32), intent(in) :: actual(:, :)
        real(real32), intent(in) :: expected(:, :)
        character(*), intent(in) :: test_name
        real(real32), intent(in), optional :: tolerance
        logical, intent(in), optional :: verbose

        logical :: test_stat

        call expect_equal(actual, expected, test_name, test_stat, tolerance, verbose)
        call stop_on_failure(test_stat)
    end subroutine assert_approxequal_2dReal32_2dReal32

    !| ランク3の単精度実数配列同士の値が等しいかを検査する<br>
    ! 許容誤差は`tolerance`で指定し，指定されない時は
    ! マシンイプシロンを使用する．<br>
    ! 指定された誤差の範囲内で同じであれば，
    ! `PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する．<br>
    ! テストが失敗したときか，`verbose`が`.true.`に
    ! 設定されている場合に，詳細を表示する
    subroutine assert_approxequal_3dReal32_3dReal32(actual, expected, test_name, tolerance, verbose)
        use :: assertion_sameShape_expect
        use :: assert_equal_compareArrayValues
        use :: assertion_common_setTolerance
        implicit none

        real(real32), intent(in) :: actual(:, :, :)
        real(real32), intent(in) :: expected(:, :, :)
        character(*), intent(in) :: test_name
        real(real32), intent(in), optional :: tolerance
        logical, intent(in), optional :: verbose

        logical :: test_stat

        call expect_equal(actual, expected, test_name, test_stat, tolerance, verbose)
        call stop_on_failure(test_stat)
    end subroutine assert_approxequal_3dReal32_3dReal32

    !| ランク1の倍精度実数配列同士の値が等しいかを検査する<br>
    ! 許容誤差は`tolerance`で指定し，指定されない時は
    ! マシンイプシロンを使用する．<br>
    ! 指定された誤差の範囲内で同じであれば，
    ! `PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する．<br>
    ! テストが失敗したときか，`verbose`が`.true.`に
    ! 設定されている場合に，詳細を表示する
    subroutine assert_approxequal_1dReal64_1dReal64(actual, expected, test_name, tolerance, verbose)
        use :: assertion_sameShape_expect
        use :: assert_equal_compareArrayValues
        use :: assertion_common_setTolerance
        implicit none

        real(real64), intent(in) :: actual(:)
        real(real64), intent(in) :: expected(:)
        character(*), intent(in) :: test_name
        real(real64), intent(in), optional :: tolerance
        logical, intent(in), optional :: verbose

        logical :: test_stat

        call expect_equal(actual, expected, test_name, test_stat, tolerance, verbose)
        call stop_on_failure(test_stat)
    end subroutine assert_approxequal_1dReal64_1dReal64

    !| ランク2の倍精度実数配列同士の値が等しいかを検査する<br>
    ! 許容誤差は`tolerance`で指定し，指定されない時は
    ! マシンイプシロンを使用する．<br>
    ! 指定された誤差の範囲内で同じであれば，
    ! `PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する．<br>
    ! テストが失敗したときか，`verbose`が`.true.`に
    ! 設定されている場合に，詳細を表示する
    subroutine assert_approxequal_2dReal64_2dReal64(actual, expected, test_name, tolerance, verbose)
        use :: assertion_sameShape_expect
        use :: assert_equal_compareArrayValues
        use :: assertion_common_setTolerance
        implicit none

        real(real64), intent(in) :: actual(:, :)
        real(real64), intent(in) :: expected(:, :)
        character(*), intent(in) :: test_name
        real(real64), intent(in), optional :: tolerance
        logical, intent(in), optional :: verbose

        logical :: test_stat

        call expect_equal(actual, expected, test_name, test_stat, tolerance, verbose)
        call stop_on_failure(test_stat)
    end subroutine assert_approxequal_2dReal64_2dReal64

    !| ランク3の倍精度実数配列同士の値が等しいかを検査する<br>
    ! 許容誤差は`tolerance`で指定し，指定されない時は
    ! マシンイプシロンを使用する．<br>
    ! 指定された誤差の範囲内で同じであれば，
    ! `PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する．<br>
    ! テストが失敗したときか，`verbose`が`.true.`に
    ! 設定されている場合に，詳細を表示する
    subroutine assert_approxequal_3dReal64_3dReal64(actual, expected, test_name, tolerance, verbose)
        use :: assertion_sameShape_expect
        use :: assert_equal_compareArrayValues
        use :: assertion_common_setTolerance
        implicit none

        real(real64), intent(in) :: actual(:, :, :)
        real(real64), intent(in) :: expected(:, :, :)
        character(*), intent(in) :: test_name
        real(real64), intent(in), optional :: tolerance
        logical, intent(in), optional :: verbose

        logical :: test_stat

        call expect_equal(actual, expected, test_name, test_stat, tolerance, verbose)
        call stop_on_failure(test_stat)
    end subroutine assert_approxequal_3dReal64_3dReal64

    !| 論理型変数同士の値が等しいかを検査する<br>
    ! 同じであれば，
    ! `PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する
    subroutine assert_equiv_logical_logical(actual, expected, test_name)
        use :: assertion_common_checkTrue
        implicit none

        logical, intent(in) :: actual
        logical, intent(in) :: expected
        character(*), intent(in) :: test_name

        logical :: test_stat

        call expect_equal(actual, expected, test_name, test_stat)
        call stop_on_failure(test_stat)
    end subroutine assert_equiv_logical_logical

    !| 文字列同士の値が等しいかを検査する<br>
    ! 同じであれば，
    ! `PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する
    subroutine assert_equal_str_str(actual, expected, test_name)
        use :: assertion_common_checkTrue
        implicit none

        character(*), intent(in) :: actual
        character(*), intent(in) :: expected
        character(*), intent(in) :: test_name

        logical :: test_stat

        call expect_equal(actual, expected, test_name, test_stat)
        call stop_on_failure(test_stat)
    end subroutine assert_equal_str_str

    !| ランク1の文字型配列同士の値が等しいかを検査する<br>
    ! 同じであれば，
    ! `PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する
    subroutine assert_equal_charArray_charArray(actual, expected, test_name)
        use :: assertion_sameShape_expect
        use :: assert_equal_compareArrayValues
        implicit none

        character, intent(in) :: actual(:)
        character, intent(in) :: expected(:)
        character(*), intent(in) :: test_name

        logical :: test_stat

        call expect_equal(actual, expected, test_name, test_stat)
        call stop_on_failure(test_stat)
    end subroutine assert_equal_charArray_charArray
end module assertion_equal_assertEqual

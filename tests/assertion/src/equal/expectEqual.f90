!| 変数あるいは配列の値が等しいかを検査する手続を定義したモジュール．
module expectEqual
    use, intrinsic :: iso_fortran_env
    use :: assert_common_checkTrue
    use :: assert_common_optval
    use :: assert_common_status
    use :: assertEqual_outputOnFailure
    implicit none
    private
    public :: expect_equal

    interface expect_equal
        procedure :: expect_equal_int8_int8
        procedure :: expect_equal_int16_int16
        procedure :: expect_equal_int32_int32
        procedure :: expect_equal_int64_int64
        procedure :: expect_equal_1dInt32_1dInt32
        procedure :: expect_equal_2dInt32_2dInt32
        procedure :: expect_equal_3dInt32_3dInt32
        procedure :: expect_approxequal_real32_real32
        procedure :: expect_approxequal_real64_real64
        procedure :: expect_approxequal_1dReal32_1dReal32
        procedure :: expect_approxequal_2dReal32_2dReal32
        procedure :: expect_approxequal_3dReal32_3dReal32
        procedure :: expect_approxequal_1dReal64_1dReal64
        procedure :: expect_approxequal_2dReal64_2dReal64
        procedure :: expect_approxequal_3dReal64_3dReal64
        procedure :: expect_equiv_logical_logical
        procedure :: expect_equal_str_str
        procedure :: expect_equal_charArray_charArray
    end interface

contains

    !| 1バイト整数同士の値が等しいかを検査する<br>
    ! 同じであれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納する．<br>
    ! テストが失敗したときか，`verbose`が`.true.`に
    ! 設定されている場合に，詳細を表示する
    subroutine expect_equal_int8_int8(actual, expected, test_name, stat, verbose)
        implicit none
        integer(int8), intent(in) :: actual
        integer(int8), intent(in) :: expected
        character(*), intent(in) :: test_name
        logical, intent(out), optional :: stat
        logical, intent(in), optional :: verbose

        logical :: test_stat

        call check_true(actual == expected, test_name, test_stat)

        if (present(stat)) stat = test_stat

        if (is_output_on_failure(test_stat, verbose)) call output_on_failure(actual, expected)
    end subroutine expect_equal_int8_int8

    !| 2バイト整数同士の値が等しいかを検査する<br>
    ! 同じであれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納する．<br>
    ! テストが失敗したときか，`verbose`が`.true.`に
    ! 設定されている場合に，詳細を表示する
    subroutine expect_equal_int16_int16(actual, expected, test_name, stat, verbose)
        implicit none
        integer(int16), intent(in) :: actual
        integer(int16), intent(in) :: expected
        character(*), intent(in) :: test_name
        logical, intent(out), optional :: stat
        logical, intent(in), optional :: verbose

        logical :: test_stat

        call check_true(actual == expected, test_name, test_stat)

        if (present(stat)) stat = test_stat

        if (is_output_on_failure(test_stat, verbose)) call output_on_failure(actual, expected)
    end subroutine expect_equal_int16_int16

    !| 1バイト整数同士の値が等しいかを検査する<br>
    ! 同じであれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納する．<br>
    ! テストが失敗したときか，`verbose`が`.true.`に
    ! 設定されている場合に，詳細を表示する
    subroutine expect_equal_int32_int32(actual, expected, test_name, stat, verbose)
        implicit none
        integer(int32), intent(in) :: actual
        integer(int32), intent(in) :: expected
        character(*), intent(in) :: test_name
        logical, intent(out), optional :: stat
        logical, intent(in), optional :: verbose

        logical :: test_stat

        call check_true(actual == expected, test_name, test_stat)

        if (present(stat)) stat = test_stat

        if (is_output_on_failure(test_stat, verbose)) call output_on_failure(actual, expected)
    end subroutine expect_equal_int32_int32

    !| 8バイト整数同士の値が等しいかを検査する<br>
    ! 同じであれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納する．<br>
    ! テストが失敗したときか，`verbose`が`.true.`に
    ! 設定されている場合に，詳細を表示する
    subroutine expect_equal_int64_int64(actual, expected, test_name, stat, verbose)
        implicit none
        integer(int64), intent(in) :: actual
        integer(int64), intent(in) :: expected
        character(*), intent(in) :: test_name
        logical, intent(out), optional :: stat
        logical, intent(in), optional :: verbose

        logical :: test_stat

        call check_true(actual == expected, test_name, test_stat)

        if (present(stat)) stat = test_stat

        if (is_output_on_failure(test_stat, verbose)) call output_on_failure(actual, expected)
    end subroutine expect_equal_int64_int64

    !| ランク1の4バイト整数型配列同士の値が等しいかを検査する<br>
    ! 同じであれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納する．
    subroutine expect_equal_1dInt32_1dInt32(actual, expected, test_name, stat)
        use :: expectSameShape
        use :: assert_equal_compareArrayValues
        implicit none

        integer(int32), intent(in) :: actual(:)
        integer(int32), intent(in) :: expected(:)
        character(*), intent(in) :: test_name
        logical, intent(out), optional :: stat

        logical :: test_stat

        call expect_same_shape(actual, expected, test_name//" (shape check)", test_stat)
        if (.not. test_stat) then
            if (present(stat)) stat = failure
            return
        end if

        test_stat = failure
        if (are_values_of_all_elements_equal(actual, expected)) test_stat = success

        call check_true(test_stat, test_name, stat)
    end subroutine expect_equal_1dInt32_1dInt32

    !| ランク2の4バイト整数型配列同士の値が等しいかを検査する<br>
    ! 同じであれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納する．
    subroutine expect_equal_2dInt32_2dInt32(actual, expected, test_name, stat)
        use :: expectSameShape
        use :: assert_equal_compareArrayValues
        implicit none

        integer(int32), intent(in) :: actual(:, :)
        integer(int32), intent(in) :: expected(:, :)
        character(*), intent(in) :: test_name
        logical, intent(out), optional :: stat

        logical :: test_stat

        call expect_same_shape(actual, expected, test_name//" (shape check)", test_stat)
        if (.not. test_stat) then
            if (present(stat)) stat = failure
            return
        end if

        test_stat = failure
        if (are_values_of_all_elements_equal(actual, expected)) test_stat = success

        call check_true(test_stat, test_name, stat)
    end subroutine expect_equal_2dInt32_2dInt32

    !| ランク3の4バイト整数型配列同士の値が等しいかを検査する<br>
    ! 同じであれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納する．
    subroutine expect_equal_3dInt32_3dInt32(actual, expected, test_name, stat)
        use :: expectSameShape
        use :: assert_equal_compareArrayValues
        implicit none

        integer(int32), intent(in) :: actual(:, :, :)
        integer(int32), intent(in) :: expected(:, :, :)
        character(*), intent(in) :: test_name
        logical, intent(out), optional :: stat

        logical :: test_stat

        call expect_same_shape(actual, expected, test_name//" (shape check)", test_stat)
        if (.not. test_stat) then
            if (present(stat)) stat = failure
            return
        end if

        test_stat = failure
        if (are_values_of_all_elements_equal(actual, expected)) test_stat = success

        call check_true(test_stat, test_name, stat)
    end subroutine expect_equal_3dInt32_3dInt32

    !| 単精度実数同士の値が等しいかを検査する<br>
    ! 許容誤差は`tolerance`で指定し，指定されない時は
    ! マシンイプシロンを使用する．<br>
    ! 指定された誤差の範囲内で同じであれば，
    ! `PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納する．<br>
    ! テストが失敗したときか，`verbose`が`.true.`に
    ! 設定されている場合に，詳細を表示する
    subroutine expect_approxequal_real32_real32(actual, expected, test_name, stat, tolerance, verbose)
        use :: assert_common_setTolerance
        implicit none

        real(real32), intent(in) :: actual
        real(real32), intent(in) :: expected
        character(*), intent(in) :: test_name
        logical, intent(out), optional :: stat
        real(real32), intent(in), optional :: tolerance
        logical, intent(in), optional :: verbose

        logical :: test_stat
        real(real32) :: tol
        tol = set_tolerance(tolerance, default=epsilon(actual))

        call check_true(abs(actual - expected) <= tol, test_name, test_stat)
        if (present(stat)) stat = test_stat

        if (is_output_on_failure(test_stat, verbose)) call output_on_failure(actual, expected)
    end subroutine expect_approxequal_real32_real32

    !| 倍精度実数同士の値が等しいかを検査する<br>
    ! 許容誤差は`tolerance`で指定し，指定されない時は
    ! マシンイプシロンを使用する．<br>
    ! 指定された誤差の範囲内で同じであれば，
    ! `PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納する．<br>
    ! テストが失敗したときか，`verbose`が`.true.`に
    ! 設定されている場合に，詳細を表示する
    subroutine expect_approxequal_real64_real64(actual, expected, test_name, stat, tolerance, verbose)
        use :: assert_common_setTolerance
        implicit none

        real(real64), intent(in) :: actual
        real(real64), intent(in) :: expected
        character(*), intent(in) :: test_name
        logical, intent(out), optional :: stat
        real(real64), intent(in), optional :: tolerance
        logical, intent(in), optional :: verbose

        logical :: test_stat
        real(real64) :: tol
        tol = set_tolerance(tolerance, default=epsilon(actual))

        call check_true(abs(actual - expected) <= tol, test_name, test_stat)
        if (present(stat)) stat = test_stat

        if (is_output_on_failure(test_stat, verbose)) call output_on_failure(actual, expected)
    end subroutine expect_approxequal_real64_real64

    !| ランク1の単精度実数配列同士の値が等しいかを検査する<br>
    ! 許容誤差は`tolerance`で指定し，指定されない時は
    ! マシンイプシロンを使用する．<br>
    ! 指定された誤差の範囲内で同じであれば，
    ! `PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納する．<br>
    ! テストが失敗したときか，`verbose`が`.true.`に
    ! 設定されている場合に，詳細を表示する
    subroutine expect_approxequal_1dReal32_1dReal32(actual, expected, test_name, stat, tolerance, verbose)
        use :: expectSameShape
        use :: assert_equal_compareArrayValues
        use :: assert_common_setTolerance
        implicit none

        real(real32), intent(in) :: actual(:)
        real(real32), intent(in) :: expected(:)
        character(*), intent(in) :: test_name
        logical, intent(out), optional :: stat
        real(real32), intent(in), optional :: tolerance
        logical, intent(in), optional :: verbose

        logical :: test_stat
        real(real32) :: tol
        tol = set_tolerance(tolerance, default=epsilon(actual))

        call expect_same_shape(actual, expected, test_name//" (shape check)", test_stat)
        if (.not. test_stat) then
            if (present(stat)) stat = failure
            return
        end if

        test_stat = failure
        if (are_values_of_all_elements_equal(actual, expected, tol)) test_stat = success

        call check_true(test_stat, test_name, stat)

        if (is_output_on_failure(test_stat, verbose)) call output_on_failure(actual, expected)
    end subroutine expect_approxequal_1dReal32_1dReal32

    !| ランク2の単精度実数配列同士の値が等しいかを検査する<br>
    ! 許容誤差は`tolerance`で指定し，指定されない時は
    ! マシンイプシロンを使用する．<br>
    ! 指定された誤差の範囲内で同じであれば，
    ! `PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納する．<br>
    ! テストが失敗したときか，`verbose`が`.true.`に
    ! 設定されている場合に，詳細を表示する
    subroutine expect_approxequal_2dReal32_2dReal32(actual, expected, test_name, stat, tolerance, verbose)
        use :: expectSameShape
        use :: assert_equal_compareArrayValues
        use :: assert_common_setTolerance
        implicit none

        real(real32), intent(in) :: actual(:, :)
        real(real32), intent(in) :: expected(:, :)
        character(*), intent(in) :: test_name
        logical, intent(out), optional :: stat
        real(real32), intent(in), optional :: tolerance
        logical, intent(in), optional :: verbose

        logical :: test_stat
        real(real32) :: tol
        tol = set_tolerance(tolerance, default=epsilon(actual))

        call expect_same_shape(actual, expected, test_name//" (shape check)", test_stat)
        if (.not. test_stat) then
            if (present(stat)) stat = failure
            return
        end if

        test_stat = failure
        if (are_values_of_all_elements_equal(actual, expected, tol)) test_stat = success

        call check_true(test_stat, test_name, stat)

        if (is_output_on_failure(test_stat, verbose)) call output_on_failure(actual, expected)
    end subroutine expect_approxequal_2dReal32_2dReal32

    !| ランク3の単精度実数配列同士の値が等しいかを検査する<br>
    ! 許容誤差は`tolerance`で指定し，指定されない時は
    ! マシンイプシロンを使用する．<br>
    ! 指定された誤差の範囲内で同じであれば，
    ! `PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納する．<br>
    ! テストが失敗したときか，`verbose`が`.true.`に
    ! 設定されている場合に，詳細を表示する
    subroutine expect_approxequal_3dReal32_3dReal32(actual, expected, test_name, stat, tolerance, verbose)
        use :: expectSameShape
        use :: assert_equal_compareArrayValues
        use :: assert_common_setTolerance
        implicit none

        real(real32), intent(in) :: actual(:, :, :)
        real(real32), intent(in) :: expected(:, :, :)
        character(*), intent(in) :: test_name
        logical, intent(out), optional :: stat
        real(real32), intent(in), optional :: tolerance
        logical, intent(in), optional :: verbose

        logical :: test_stat
        real(real32) :: tol
        tol = set_tolerance(tolerance, default=epsilon(actual))

        call expect_same_shape(actual, expected, test_name//" (shape check)", test_stat)
        if (.not. test_stat) then
            if (present(stat)) stat = failure
            return
        end if

        test_stat = failure
        if (are_values_of_all_elements_equal(actual, expected, tol)) test_stat = success

        call check_true(test_stat, test_name, stat)

        if (is_output_on_failure(test_stat, verbose)) call output_on_failure(actual, expected)
    end subroutine expect_approxequal_3dReal32_3dReal32

    !| ランク1の倍精度実数配列同士の値が等しいかを検査する<br>
    ! 許容誤差は`tolerance`で指定し，指定されない時は
    ! マシンイプシロンを使用する．<br>
    ! 指定された誤差の範囲内で同じであれば，
    ! `PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納する．<br>
    ! テストが失敗したときか，`verbose`が`.true.`に
    ! 設定されている場合に，詳細を表示する
    subroutine expect_approxequal_1dReal64_1dReal64(actual, expected, test_name, stat, tolerance, verbose)
        use :: expectSameShape
        use :: assert_equal_compareArrayValues
        use :: assert_common_setTolerance
        implicit none

        real(real64), intent(in) :: actual(:)
        real(real64), intent(in) :: expected(:)
        character(*), intent(in) :: test_name
        logical, intent(out), optional :: stat
        real(real64), intent(in), optional :: tolerance
        logical, intent(in), optional :: verbose

        logical :: test_stat
        real(real64) :: tol
        tol = set_tolerance(tolerance, default=epsilon(actual))

        call expect_same_shape(actual, expected, test_name//" (shape check)", test_stat)
        if (.not. test_stat) then
            if (present(stat)) stat = failure
            return
        end if

        test_stat = failure
        if (are_values_of_all_elements_equal(actual, expected, tol)) test_stat = success

        call check_true(test_stat, test_name, stat)

        if (is_output_on_failure(test_stat, verbose)) call output_on_failure(actual, expected)
    end subroutine expect_approxequal_1dReal64_1dReal64

    !| ランク2の倍精度実数配列同士の値が等しいかを検査する<br>
    ! 許容誤差は`tolerance`で指定し，指定されない時は
    ! マシンイプシロンを使用する．<br>
    ! 指定された誤差の範囲内で同じであれば，
    ! `PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納する．<br>
    ! テストが失敗したときか，`verbose`が`.true.`に
    ! 設定されている場合に，詳細を表示する
    subroutine expect_approxequal_2dReal64_2dReal64(actual, expected, test_name, stat, tolerance, verbose)
        use :: expectSameShape
        use :: assert_equal_compareArrayValues
        use :: assert_common_setTolerance
        implicit none

        real(real64), intent(in) :: actual(:, :)
        real(real64), intent(in) :: expected(:, :)
        character(*), intent(in) :: test_name
        logical, intent(out), optional :: stat
        real(real64), intent(in), optional :: tolerance
        logical, intent(in), optional :: verbose

        logical :: test_stat
        real(real64) :: tol
        tol = set_tolerance(tolerance, default=epsilon(actual))

        call expect_same_shape(actual, expected, test_name//" (shape check)", test_stat)
        if (.not. test_stat) then
            if (present(stat)) stat = failure
            return
        end if

        test_stat = failure
        if (are_values_of_all_elements_equal(actual, expected, tol)) test_stat = success

        call check_true(test_stat, test_name, stat)

        if (is_output_on_failure(test_stat, verbose)) call output_on_failure(actual, expected)
    end subroutine expect_approxequal_2dReal64_2dReal64

    !| ランク3の倍精度実数配列同士の値が等しいかを検査する<br>
    ! 許容誤差は`tolerance`で指定し，指定されない時は
    ! マシンイプシロンを使用する．<br>
    ! 指定された誤差の範囲内で同じであれば，
    ! `PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納する．<br>
    ! テストが失敗したときか，`verbose`が`.true.`に
    ! 設定されている場合に，詳細を表示する
    subroutine expect_approxequal_3dReal64_3dReal64(actual, expected, test_name, stat, tolerance, verbose)
        use :: expectSameShape
        use :: assert_equal_compareArrayValues
        use :: assert_common_setTolerance
        implicit none

        real(real64), intent(in) :: actual(:, :, :)
        real(real64), intent(in) :: expected(:, :, :)
        character(*), intent(in) :: test_name
        logical, intent(out), optional :: stat
        real(real64), intent(in), optional :: tolerance
        logical, intent(in), optional :: verbose

        logical :: test_stat
        real(real64) :: tol
        tol = set_tolerance(tolerance, default=epsilon(actual))

        call expect_same_shape(actual, expected, test_name//" (shape check)", test_stat)
        if (.not. test_stat) then
            if (present(stat)) stat = failure
            return
        end if

        test_stat = failure
        if (are_values_of_all_elements_equal(actual, expected, tol)) test_stat = success

        call check_true(test_stat, test_name, stat)

        if (is_output_on_failure(test_stat, verbose)) call output_on_failure(actual, expected)
    end subroutine expect_approxequal_3dReal64_3dReal64

    !| 論理型変数同士の値が等しいかを検査する<br>
    ! 同じであれば，
    ! `PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納する．
    subroutine expect_equiv_logical_logical(actual, expected, test_name, stat)
        use :: assert_common_checkTrue
        implicit none

        logical, intent(in) :: actual
        logical, intent(in) :: expected
        character(*), intent(in) :: test_name
        logical, intent(out), optional :: stat

        call check_true(actual .eqv. expected, test_name, stat)
    end subroutine expect_equiv_logical_logical

    !| 文字列同士の値が等しいかを検査する<br>
    ! 同じであれば，
    ! `PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納する．
    subroutine expect_equal_str_str(actual, expected, test_name, stat)
        use :: assert_common_checkTrue
        implicit none

        character(*), intent(in) :: actual
        character(*), intent(in) :: expected
        character(*), intent(in) :: test_name
        logical, intent(out), optional :: stat

        logical :: is_same_length

        call expect_equal(len(actual), len(expected), test_name//" (length check)", is_same_length)
        if (.not. is_same_length) then
            if (present(stat)) stat = failure
            return
        end if
        call check_true(is_same_length .and. (actual == expected), test_name, stat)
    end subroutine expect_equal_str_str

    !| ランク1の文字型配列同士の値が等しいかを検査する<br>
    ! 同じであれば，
    ! `PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! 値が異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納する．
    subroutine expect_equal_charArray_charArray(actual, expected, test_name, stat)
        use :: expectSameShape
        use :: assert_equal_compareArrayValues
        implicit none

        character, intent(in) :: actual(:)
        character, intent(in) :: expected(:)
        character(*), intent(in) :: test_name
        logical, intent(out), optional :: stat

        logical :: test_stat

        call expect_same_shape(actual, expected, test_name//" (shape check)", test_stat)
        if (.not. test_stat) then
            if (present(stat)) stat = failure
            return
        end if

        if (are_values_of_all_elements_equal(actual, expected)) test_stat = success

        call check_true(test_stat, test_name, stat)
    end subroutine expect_equal_charArray_charArray

    !------------------------------------------------------------------!
    !| テスト失敗時に詳細を表示するかを判別する
    logical function is_output_on_failure(test_stat, verbose)
        implicit none
        logical, intent(in) :: test_stat
        logical, intent(in), optional :: verbose

        is_output_on_failure = (test_stat .eqv. failure) .or. optval(verbose, .false.)
    end function is_output_on_failure
end module expectEqual

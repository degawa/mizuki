!| 配列の形状が同じかを検査し，
! 失敗した際にプログラムを停止する手続を定義したモジュール．
module assertSameShape
    use, intrinsic :: iso_fortran_env
    use :: expectSameShape, only:expect_same_shape
    use :: assert_common_stopOnFailure
    implicit none
    private
    public :: assert_same_shape

    interface assert_same_shape
        procedure :: assert_sameshape_1d_int32
        procedure :: assert_sameshape_2d_int32
        procedure :: assert_sameshape_3d_int32
        procedure :: assert_sameshape_1d_real32
        procedure :: assert_sameshape_2d_real32
        procedure :: assert_sameshape_3d_real32
        procedure :: assert_sameshape_1d_real64
        procedure :: assert_sameshape_2d_real64
        procedure :: assert_sameshape_3d_real64
        procedure :: assert_sameshape_1d_charArray
    end interface

contains

    !| ランク1の4バイト整数型配列同士のサイズが同じかを検査する<br>
    ! 同じであれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! サイズが異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する
    subroutine assert_sameshape_1d_int32(actual, expected, test_name)
        implicit none

        integer(int32), intent(in) :: actual(:)
        integer(int32), intent(in) :: expected(:)
        character(*), intent(in) :: test_name

        logical :: stat

        call expect_same_shape(actual, expected, test_name, stat)
        call stop_on_failure(stat)
    end subroutine assert_sameshape_1d_int32

    !| ランク2の4バイト整数型配列同士のサイズが同じかを検査する<br>
    ! 同じであれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! サイズが異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する
    subroutine assert_sameshape_2d_int32(actual, expected, test_name)
        implicit none

        integer(int32), intent(in) :: actual(:, :)
        integer(int32), intent(in) :: expected(:, :)
        character(*), intent(in) :: test_name

        logical :: stat

        call expect_same_shape(actual, expected, test_name, stat)
        call stop_on_failure(stat)
    end subroutine assert_sameshape_2d_int32

    !| ランク3の4バイト整数型配列同士のサイズが同じかを検査する<br>
    ! 同じであれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! サイズが異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する
    subroutine assert_sameshape_3d_int32(actual, expected, test_name)
        implicit none

        integer(int32), intent(in) :: actual(:, :, :)
        integer(int32), intent(in) :: expected(:, :, :)
        character(*), intent(in) :: test_name

        logical :: stat

        call expect_same_shape(actual, expected, test_name, stat)
        call stop_on_failure(stat)
    end subroutine assert_sameshape_3d_int32

    !| ランク1の単精度実数型配列同士のサイズが同じかを検査する<br>
    ! 同じであれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! サイズが異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する
    subroutine assert_sameshape_1d_real32(actual, expected, test_name)
        implicit none

        real(real32), intent(in) :: actual(:)
        real(real32), intent(in) :: expected(:)
        character(*), intent(in) :: test_name

        logical :: stat

        call expect_same_shape(actual, expected, test_name, stat)
        call stop_on_failure(stat)
    end subroutine assert_sameshape_1d_real32

    !| ランク2の単精度実数型配列同士のサイズが同じかを検査する<br>
    ! 同じであれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! サイズが異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する
    subroutine assert_sameshape_2d_real32(actual, expected, test_name)
        implicit none

        real(real32), intent(in) :: actual(:, :)
        real(real32), intent(in) :: expected(:, :)
        character(*), intent(in) :: test_name

        logical :: stat

        call expect_same_shape(actual, expected, test_name, stat)
        call stop_on_failure(stat)
    end subroutine assert_sameshape_2d_real32

    !| ランク3の単精度実数型配列同士のサイズが同じかを検査する<br>
    ! 同じであれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! サイズが異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する
    subroutine assert_sameshape_3d_real32(actual, expected, test_name)
        implicit none

        real(real32), intent(in) :: actual(:, :, :)
        real(real32), intent(in) :: expected(:, :, :)
        character(*), intent(in) :: test_name

        logical :: stat

        call expect_same_shape(actual, expected, test_name, stat)
        call stop_on_failure(stat)
    end subroutine assert_sameshape_3d_real32

    !| ランク1の倍精度実数型配列同士のサイズが同じかを検査する<br>
    ! 同じであれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! サイズが異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する
    subroutine assert_sameshape_1d_real64(actual, expected, test_name)
        implicit none

        real(real64), intent(in) :: actual(:)
        real(real64), intent(in) :: expected(:)
        character(*), intent(in) :: test_name

        logical :: stat

        call expect_same_shape(actual, expected, test_name, stat)
        call stop_on_failure(stat)
    end subroutine assert_sameshape_1d_real64

    !| ランク2の倍精度実数型配列同士のサイズが同じかを検査する<br>
    ! 同じであれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! サイズが異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する
    subroutine assert_sameshape_2d_real64(actual, expected, test_name)
        implicit none

        real(real64), intent(in) :: actual(:, :)
        real(real64), intent(in) :: expected(:, :)
        character(*), intent(in) :: test_name

        logical :: stat

        call expect_same_shape(actual, expected, test_name, stat)
        call stop_on_failure(stat)
    end subroutine assert_sameshape_2d_real64

    !| ランク3の倍精度実数型配列同士のサイズが同じかを検査する<br>
    ! 同じであれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! サイズが異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する
    subroutine assert_sameshape_3d_real64(actual, expected, test_name)
        implicit none

        real(real64), intent(in) :: actual(:, :, :)
        real(real64), intent(in) :: expected(:, :, :)
        character(*), intent(in) :: test_name

        logical :: stat

        call expect_same_shape(actual, expected, test_name, stat)
        call stop_on_failure(stat)
    end subroutine assert_sameshape_3d_real64

    !| ランク1の文字型配列同士のサイズが同じかを検査する<br>
    ! 同じであれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! サイズが異なっていれば，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納し，
    ! プログラムを停止する
    subroutine assert_sameshape_1d_charArray(actual, expected, test_name)
        implicit none

        character, intent(in) :: actual(:)
        character, intent(in) :: expected(:)
        character(*), intent(in) :: test_name

        logical :: stat

        call expect_same_shape(actual, expected, test_name, stat)
        call stop_on_failure(stat)
    end subroutine assert_sameshape_1d_charArray
end module assertSameShape

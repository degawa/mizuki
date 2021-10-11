!| テストが失敗した際に，画面に値を表示する手続を定義したモジュール．
module assertion_equal_outputOnFailure
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: output_on_failure

    interface output_on_failure
        procedure :: output_int8_int8
        procedure :: output_int16_int16
        procedure :: output_int32_int32
        procedure :: output_int64_int64
        procedure :: output_real32_real32
        procedure :: output_real64_real64
        procedure :: output_1dReal32_1dReal32
        procedure :: output_2dReal32_2dReal32
        procedure :: output_3dReal32_3dReal32
        procedure :: output_1dReal64_1dReal64
        procedure :: output_2dReal64_2dReal64
        procedure :: output_3dReal64_3dReal64
    end interface

contains

    !| 1バイト整数の推定値と真値を表示する
    subroutine output_int8_int8(actual, expected)
        implicit none
        integer(int8), intent(in) :: actual
        integer(int8), intent(in) :: expected

        print '(4X,A,I0)', "Expected: ", expected
        print '(4X,A,I0)', "Actural : ", actual
    end subroutine output_int8_int8

    !| 2バイト整数の推定値と真値を表示する
    subroutine output_int16_int16(actual, expected)
        implicit none
        integer(int16), intent(in) :: actual
        integer(int16), intent(in) :: expected

        print '(4X,A,I0)', "Expected: ", expected
        print '(4X,A,I0)', "Actural : ", actual
    end subroutine output_int16_int16

    !| 4バイト整数の推定値と真値を表示する
    subroutine output_int32_int32(actual, expected)
        implicit none
        integer(int32), intent(in) :: actual
        integer(int32), intent(in) :: expected

        print '(4X,A,I0)', "Expected: ", expected
        print '(4X,A,I0)', "Actural : ", actual
    end subroutine output_int32_int32

    !| 8バイト整数の推定値と真値を表示する
    subroutine output_int64_int64(actual, expected)
        implicit none
        integer(int64), intent(in) :: actual
        integer(int64), intent(in) :: expected

        print '(4X,A,I0)', "Expected: ", expected
        print '(4X,A,I0)', "Actural : ", actual
    end subroutine output_int64_int64

    !| 単精度実数の推定値と真値と差を表示する
    subroutine output_real32_real32(actual, expected)
        implicit none
        real(real32), intent(in) :: actual
        real(real32), intent(in) :: expected

        print '(4X,A,g0)', "Expected: ", expected
        print '(4X,A,g0)', "Actural : ", actual
        print '(4X,A,g0)', "Difference: ", expected - actual
    end subroutine output_real32_real32

    !| 倍精度実数の推定値と真値と差を表示する
    subroutine output_real64_real64(actual, expected)
        implicit none
        real(real64), intent(in) :: actual
        real(real64), intent(in) :: expected

        print '(4X,A,g0)', "Expected: ", expected
        print '(4X,A,g0)', "Actural : ", actual
        print '(4X,A,g0)', "Difference:", expected - actual
    end subroutine output_real64_real64

    !| ランク1の単精度実数配列同士の絶対誤差を計算し，
    ! 最大値と最小値を表示する
    subroutine output_1dReal32_1dReal32(actual, expected)
        implicit none
        real(real32), intent(in) :: actual(:)
        real(real32), intent(in) :: expected(:)

        print '(4X,A,g0)', "Maximum Absolute Difference: ", maxval(abs(expected - actual))
        print '(4X,A,g0)', "Minumum Absolute Difference: ", minval(abs(expected - actual))
    end subroutine output_1dReal32_1dReal32

    !| ランク2の単精度実数配列同士の絶対誤差を計算し，
    ! 最大値と最小値を表示する
    subroutine output_2dReal32_2dReal32(actual, expected)
        implicit none
        real(real32), intent(in) :: actual(:, :)
        real(real32), intent(in) :: expected(:, :)

        print '(4X,A,g0)', "Maximum Absolute Difference: ", maxval(abs(expected - actual))
        print '(4X,A,g0)', "Minumum Absolute Difference: ", minval(abs(expected - actual))
    end subroutine output_2dReal32_2dReal32

    !| ランク3の単精度実数配列同士の絶対誤差を計算し，
    ! 最大値と最小値を表示する
    subroutine output_3dReal32_3dReal32(actual, expected)
        implicit none
        real(real32), intent(in) :: actual(:, :, :)
        real(real32), intent(in) :: expected(:, :, :)

        print '(4X,A,g0)', "Maximum Absolute Difference: ", maxval(abs(expected - actual))
        print '(4X,A,g0)', "Minumum Absolute Difference: ", minval(abs(expected - actual))
    end subroutine output_3dReal32_3dReal32

    !| ランク1の倍精度実数配列同士の絶対誤差を計算し，
    ! 最大値と最小値を表示する
    subroutine output_1dReal64_1dReal64(actual, expected)
        implicit none
        real(real64), intent(in) :: actual(:)
        real(real64), intent(in) :: expected(:)

        print '(4X,A,g0)', "Maximum Absolute Difference: ", maxval(abs(expected - actual))
        print '(4X,A,g0)', "Minumum Absolute Difference: ", minval(abs(expected - actual))
    end subroutine output_1dReal64_1dReal64

    !| ランク2の倍精度実数配列同士の絶対誤差を計算し，
    ! 最大値と最小値を表示する
    subroutine output_2dReal64_2dReal64(actual, expected)
        implicit none
        real(real64), intent(in) :: actual(:, :)
        real(real64), intent(in) :: expected(:, :)

        print '(4X,A,g0)', "Maximum Absolute Difference: ", maxval(abs(expected - actual))
        print '(4X,A,g0)', "Minumum Absolute Difference: ", minval(abs(expected - actual))
    end subroutine output_2dReal64_2dReal64

    !| ランク3の倍精度実数配列同士の絶対誤差を計算し，
    ! 最大値と最小値を表示する
    subroutine output_3dReal64_3dReal64(actual, expected)
        implicit none
        real(real64), intent(in) :: actual(:, :, :)
        real(real64), intent(in) :: expected(:, :, :)

        print '(4X,A,g0)', "Maximum Absolute Difference: ", maxval(abs(expected - actual))
        print '(4X,A,g0)', "Minumum Absolute Difference: ", minval(abs(expected - actual))
    end subroutine output_3dReal64_3dReal64
end module assertion_equal_outputOnFailure

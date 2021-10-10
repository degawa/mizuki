!| 配列の形状を比較する手続を定義したモジュール．
module expect_sameShape_compareArrayShape
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: are_same_shape

    interface are_same_shape
        procedure :: compare_shape_1d_int32
        procedure :: compare_shape_2d_int32
        procedure :: compare_shape_3d_int32
        procedure :: compare_shape_1d_real32
        procedure :: compare_shape_2d_real32
        procedure :: compare_shape_3d_real32
        procedure :: compare_shape_1d_real64
        procedure :: compare_shape_2d_real64
        procedure :: compare_shape_3d_real64
        procedure :: compare_shape_1d_char
    end interface

contains

    !| ランク1の4バイト整数型配列同士のサイズを比較する
    pure function compare_shape_1d_int32(array1, array2) result(are_same)
        implicit none

        integer(int32), intent(in) :: array1(:)
        integer(int32), intent(in) :: array2(:)
        logical :: are_same

        are_same = all(shape(array1) == shape(array2))
    end function compare_shape_1d_int32

    !| ランク2の4バイト整数型配列同士のサイズを比較する
    pure function compare_shape_2d_int32(array1, array2) result(are_same)
        implicit none

        integer(int32), intent(in) :: array1(:, :)
        integer(int32), intent(in) :: array2(:, :)
        logical :: are_same

        are_same = all(shape(array1) == shape(array2))
    end function compare_shape_2d_int32

    !| ランク3の4バイト整数型配列同士のサイズを比較する
    pure function compare_shape_3d_int32(array1, array2) result(are_same)
        implicit none

        integer(int32), intent(in) :: array1(:, :, :)
        integer(int32), intent(in) :: array2(:, :, :)
        logical :: are_same

        are_same = all(shape(array1) == shape(array2))
    end function compare_shape_3d_int32

    !| ランク1の単精度実数型配列同士のサイズを比較する
    pure function compare_shape_1d_real32(array1, array2) result(are_same)
        implicit none

        real(real32), intent(in) :: array1(:)
        real(real32), intent(in) :: array2(:)
        logical :: are_same

        are_same = all(shape(array1) == shape(array2))
    end function compare_shape_1d_real32

    !| ランク2の単精度実数型配列同士のサイズを比較する
    pure function compare_shape_2d_real32(array1, array2) result(are_same)
        implicit none

        real(real32), intent(in) :: array1(:, :)
        real(real32), intent(in) :: array2(:, :)
        logical :: are_same

        are_same = all(shape(array1) == shape(array2))
    end function compare_shape_2d_real32

    !| ランク3の単精度実数型配列同士のサイズを比較する
    pure function compare_shape_3d_real32(array1, array2) result(are_same)
        implicit none

        real(real32), intent(in) :: array1(:, :, :)
        real(real32), intent(in) :: array2(:, :, :)
        logical :: are_same

        are_same = all(shape(array1) == shape(array2))
    end function compare_shape_3d_real32

    !| ランク1の単精度実数型配列同士のサイズを比較する
    pure function compare_shape_1d_real64(array1, array2) result(are_same)
        implicit none

        real(real64), intent(in) :: array1(:)
        real(real64), intent(in) :: array2(:)
        logical :: are_same

        are_same = all(shape(array1) == shape(array2))
    end function compare_shape_1d_real64

    !| ランク2の単精度実数型配列同士のサイズを比較する
    pure function compare_shape_2d_real64(array1, array2) result(are_same)
        implicit none

        real(real64), intent(in) :: array1(:, :)
        real(real64), intent(in) :: array2(:, :)
        logical :: are_same

        are_same = all(shape(array1) == shape(array2))
    end function compare_shape_2d_real64

    !| ランク3の単精度実数型配列同士のサイズを比較する
    pure function compare_shape_3d_real64(array1, array2) result(are_same)
        implicit none

        real(real64), intent(in) :: array1(:, :, :)
        real(real64), intent(in) :: array2(:, :, :)
        logical :: are_same

        are_same = all(shape(array1) == shape(array2))
    end function compare_shape_3d_real64

    !| ランク1の文字型配列同士のサイズを比較する
    pure function compare_shape_1d_char(array1, array2) result(are_same)
        implicit none

        character, intent(in) :: array1(:)
        character, intent(in) :: array2(:)
        logical :: are_same

        are_same = all(shape(array1) == shape(array2))
    end function compare_shape_1d_char
end module expect_sameShape_compareArrayShape

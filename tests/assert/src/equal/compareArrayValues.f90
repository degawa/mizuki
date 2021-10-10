!| 配列同士の値を比較する手続を定義したモジュール．
module assert_equal_compareArrayValues
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: are_values_of_all_elements_equal

    interface are_values_of_all_elements_equal
        procedure :: compare_all_values_1d_int32
        procedure :: compare_all_values_2d_int32
        procedure :: compare_all_values_3d_int32
        procedure :: compare_all_values_1d_real32
        procedure :: compare_all_values_2d_real32
        procedure :: compare_all_values_3d_real32
        procedure :: compare_all_values_1d_real64
        procedure :: compare_all_values_2d_real64
        procedure :: compare_all_values_3d_real64
        procedure :: compare_all_values_1d_charaArray
    end interface

contains
    !| ランク1の4バイト整数型配列同士の値を比較する
    pure function compare_all_values_1d_int32(array1, array2) result(are_same)
        implicit none
        integer(int32), intent(in) :: array1(:)
        integer(int32), intent(in) :: array2(:)
        logical :: are_same

        are_same = all(array1 == array2)
    end function compare_all_values_1d_int32

    !| ランク2の4バイト整数型配列同士の値を比較する
    pure function compare_all_values_2d_int32(array1, array2) result(are_same)
        implicit none
        integer(int32), intent(in) :: array1(:, :)
        integer(int32), intent(in) :: array2(:, :)
        logical :: are_same

        are_same = all(array1 == array2)
    end function compare_all_values_2d_int32

    !| ランク3の4バイト整数型配列同士の値を比較する
    pure function compare_all_values_3d_int32(array1, array2) result(are_same)
        implicit none
        integer(int32), intent(in) :: array1(:, :, :)
        integer(int32), intent(in) :: array2(:, :, :)
        logical :: are_same

        are_same = all(array1 == array2)
    end function compare_all_values_3d_int32

    !| ランク1の単精度実数配列同士の値を比較する<br>
    ! 許容誤差は`tolerance`で指定し，指定されない時は
    ! マシンイプシロンを使用する
    pure function compare_all_values_1d_real32(array1, array2, tolerance) result(are_same)
        use :: assert_common_setTolerance
        implicit none
        real(real32), intent(in) :: array1(:)
        real(real32), intent(in) :: array2(:)
        real(real32), intent(in), optional :: tolerance
        logical :: are_same

        real(real32) :: tol
        tol = set_tolerance(tolerance, default=epsilon(array1))

        are_same = all(abs(array1 - array2) <= tol)
    end function compare_all_values_1d_real32

    !| ランク2の単精度実数配列同士の値を比較する<br>
    ! 許容誤差は`tolerance`で指定し，指定されない時は
    ! マシンイプシロンを使用する
    pure function compare_all_values_2d_real32(array1, array2, tolerance) result(are_same)
        use :: assert_common_setTolerance
        implicit none
        real(real32), intent(in) :: array1(:, :)
        real(real32), intent(in) :: array2(:, :)
        real(real32), intent(in), optional :: tolerance
        logical :: are_same

        real(real32) :: tol
        tol = set_tolerance(tolerance, default=epsilon(array1))

        are_same = all(abs(array1 - array2) <= tol)
    end function compare_all_values_2d_real32

    !| ランク3の単精度実数配列同士の値を比較する<br>
    ! 許容誤差は`tolerance`で指定し，指定されない時は
    ! マシンイプシロンを使用する
    pure function compare_all_values_3d_real32(array1, array2, tolerance) result(are_same)
        use :: assert_common_setTolerance
        implicit none
        real(real32), intent(in) :: array1(:, :, :)
        real(real32), intent(in) :: array2(:, :, :)
        real(real32), intent(in), optional :: tolerance
        logical :: are_same

        real(real32) :: tol
        tol = set_tolerance(tolerance, default=epsilon(array1))

        are_same = all(abs(array1 - array2) <= tol)
    end function compare_all_values_3d_real32

    !| ランク1の倍精度実数配列同士の値を比較する<br>
    ! 許容誤差は`tolerance`で指定し，指定されない時は
    ! マシンイプシロンを使用する
    pure function compare_all_values_1d_real64(array1, array2, tolerance) result(are_same)
        use :: assert_common_setTolerance
        implicit none
        real(real64), intent(in) :: array1(:)
        real(real64), intent(in) :: array2(:)
        real(real64), intent(in), optional :: tolerance
        logical :: are_same

        real(real64) :: tol
        tol = set_tolerance(tolerance, default=epsilon(array1))

        are_same = all(abs(array1 - array2) <= tol)
    end function compare_all_values_1d_real64

    !| ランク2の倍精度実数配列同士の値を比較する<br>
    ! 許容誤差は`tolerance`で指定し，指定されない時は
    ! マシンイプシロンを使用する
    pure function compare_all_values_2d_real64(array1, array2, tolerance) result(are_same)
        use :: assert_common_setTolerance
        implicit none
        real(real64), intent(in) :: array1(:, :)
        real(real64), intent(in) :: array2(:, :)
        real(real64), intent(in), optional :: tolerance
        logical :: are_same

        real(real64) :: tol
        tol = set_tolerance(tolerance, default=epsilon(array1))

        are_same = all(abs(array1 - array2) <= tol)
    end function compare_all_values_2d_real64

    !| ランク3の倍精度実数配列同士の値を比較する<br>
    ! 許容誤差は`tolerance`で指定し，指定されない時は
    ! マシンイプシロンを使用する
    pure function compare_all_values_3d_real64(array1, array2, tolerance) result(are_same)
        use :: assert_common_setTolerance
        implicit none
        real(real64), intent(in) :: array1(:, :, :)
        real(real64), intent(in) :: array2(:, :, :)
        real(real64), intent(in), optional :: tolerance
        logical :: are_same

        real(real64) :: tol
        tol = set_tolerance(tolerance, default=epsilon(array1))

        are_same = all(abs(array1 - array2) <= tol)
    end function compare_all_values_3d_real64

    !| ランク1の文字型配列同士の値を比較する
    pure function compare_all_values_1d_charaArray(array1, array2) result(are_same)
        use :: assert_common_setTolerance
        implicit none
        character, intent(in) :: array1(:)
        character, intent(in) :: array2(:)
        logical :: are_same

        integer(int32) :: i
        do i = 1, size(array2)
            if (array1(i) /= array2(i)) then
                are_same = .false.
                return
            end if
        end do
        are_same = .true.
    end function compare_all_values_1d_charaArray
end module assert_equal_compareArrayValues

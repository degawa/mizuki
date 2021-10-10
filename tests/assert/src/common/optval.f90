!| `optional`な引数の利便性を改善する`optval`を定義したモジュール．<br>
! `optional`な引数と標準値を渡し，`optional`な引数が渡されていれば
! その値を，渡されていなければ標準値を返す
module assert_common_optval
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: optval

    interface optval
        procedure :: optval_logical
        procedure :: optval_real32
        procedure :: optval_real64
    end interface

contains

    !| 論理型の引数に対する`optval`
    pure function optval_logical(x, default) result(y)
        implicit none
        logical, intent(in), optional :: x
        logical, intent(in) :: default

        logical :: y

        if (.not. present(x)) then
            y = default
        else
            y = x
        end if
    end function optval_logical

    !| 単精度実数型の引数に対する`optval`
    pure function optval_real32(x, default) result(y)
        implicit none
        real(real32), intent(in), optional :: x
        real(real32), intent(in) :: default

        real(real32) :: y

        if (.not. present(x)) then
            y = default
        else
            y = x
        end if
    end function optval_real32

    !| 倍精度実数型の引数に対する`optval`
    pure function optval_real64(x, default) result(y)
        implicit none
        real(real64), intent(in), optional :: x
        real(real64), intent(in) :: default

        real(real64) :: y

        if (.not. present(x)) then
            y = default
        else
            y = x
        end if
    end function optval_real64
end module assert_common_optval

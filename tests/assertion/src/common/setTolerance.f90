!| 浮動小数点数の等価比較を行う際に許容される誤差を
! 設定する手続を定義したモジュール
!
!@note
! やっていることはstdlibの`optval`と同じなので，
! stdlibを用いればこの手続は不要であるが，
! stdlib自体のテストに使うことを想定すると，
! 外部ライブラリに依存しない事が必要
!@endnote
!
module assertion_common_setTolerance
    use, intrinsic :: iso_fortran_env
    use :: assertion_common_optval
    implicit none
    private
    public :: set_tolerance

    interface set_tolerance
        procedure :: set_tolerance_real32
        procedure :: set_tolerance_real64
    end interface

contains
    !| 単精度実数の等価比較を行う際に
    ! 許容される誤差を設定する<br>
    ! 第1引数`tol32`が渡されない場合は，第2引数`default`
    ! の値が採用される
    pure function set_tolerance_real32(tol32, default) result(tol)
        implicit none
        !&<
        real(real32), intent(in), optional  :: tol32
            !! 許容誤差
        real(real32), intent(in)            :: default
            !! 許容誤差の標準値
        !&>

        real(real32) :: tol
            !! 設定された許容誤差

        tol = optval(tol32, default)
    end function set_tolerance_real32

    !| 倍精度実数の等価比較を行う際に
    ! 許容される誤差を設定する<br>
    ! 第1引数`tol64`が渡されない場合は，第2引数`default`
    ! の値が採用される
    !
    pure function set_tolerance_real64(tol64, default) result(tol)
        implicit none
        !&<
        real(real64), intent(in), optional  :: tol64
            !! 許容誤差
        real(real64), intent(in)            :: default
            !! 許容誤差の標準値
        !&>
        real(real64) :: tol
            !! 設定された許容誤差

        tol = optval(tol64, default)
    end function set_tolerance_real64
end module assertion_common_setTolerance

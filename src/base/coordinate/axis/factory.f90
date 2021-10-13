!| 座標系の各軸の情報（最小値と最大値）を取り扱うaxis型
! の変数を返すファクトリを定義したモジュール<br>
! axis型は成分が`private`でbuilt-inコンストラクタでは
! 初期化できないため，ファクトリが必要
module coordinate_axis_factory
    use, intrinsic :: iso_fortran_env
    use :: coordinate_axis, only:axis
    implicit none
    private
    public :: axis_factory

    interface axis_factory
        procedure :: axis_factory_values
        procedure :: axis_factory_array
    end interface

contains
    !| 引数で指定した値に基づいて初期化したaxis型変数を返す手続
    function axis_factory_values(min_coord_val, max_coord_val) result(new_axis)
        implicit none
        real(real64), intent(in) :: min_coord_val
            !! 軸の最小値
        real(real64), intent(in) :: max_coord_val
            !! 軸の最大値

        type(axis) :: new_axis

        call new_axis%construct(min_coord_val=min_coord_val, max_coord_val=max_coord_val)
    end function axis_factory_values

    !| 引数で指定した配列の要素に基づいて初期化したaxis型変数を返す手続
    function axis_factory_array(coord_vals) result(new_axis)
        implicit none
        real(real64), intent(in) :: coord_vals(2)
             !! 軸の最小値と最大値<br> `[min, max]`の順に格納

        type(axis) :: new_axis

        call new_axis%construct(coord_vals=coord_vals)
    end function axis_factory_array
end module coordinate_axis_factory

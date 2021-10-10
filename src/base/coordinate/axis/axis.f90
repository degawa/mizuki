!| 座標系の各軸の情報（最小値と最大値）を取り扱うモジュール．
module coordinate_axis
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    implicit none
    private
    public :: min_coord, max_coord

    enum, bind(c)
        enumerator :: min_coord = 1
        enumerator :: max_coord
    end enum

    type, public :: axis
        real(real64), private :: min
            !! 軸の最小値
        real(real64), private :: max
            !! 軸の最大値
    contains
        procedure, public, pass :: set_coord_values
        procedure, public, pass :: get_coord_values
        procedure, public, pass :: set_minimum_coord_value
        procedure, public, pass :: get_minimum_coord_value
        procedure, public, pass :: set_maximum_coord_value
        procedure, public, pass :: get_maximum_coord_value
    end type axis

contains

    !| 軸の最小値と最大値を設定する．
    subroutine set_coord_values(this, coord_vals)
        implicit none
        !&<
        class(axis) , intent(inout) :: this
            !! 当該実体仮引数
        real(real64), intent(in)    :: coord_vals(2)
            !! 軸の最小値と最大値 `[min, max]`の順に格納
        !&>

        !|@todo
        ! coord_vals(min_coord) > coord_vals(max_coord)
        ! の場合のテストの作成
        !@endtodo

        this%min = coord_vals(min_coord)
        this%max = coord_vals(max_coord)
    end subroutine set_coord_values

    !| 軸の最小値と最大値を取得する．
    function get_coord_values(this) result(coord_vals)
        implicit none

        class(axis), intent(in) :: this
            !! 当該実体仮引数

        real(real64) :: coord_vals(2)
            !! 軸の最小値と最大値 `[min, max]`の順に格納

        coord_vals(min_coord) = this%min
        coord_vals(max_coord) = this%max
    end function get_coord_values

    !| 軸の最小値を設定する．
    subroutine set_minimum_coord_value(this, min_coord_val)
        implicit none
        !&<
        class(axis) , intent(inout) :: this
            !! 当該実体仮引数
        real(real64), intent(in)    :: min_coord_val
            !! 軸の最小値
        !&>

        this%min = min_coord_val
    end subroutine set_minimum_coord_value

    !| 軸の最小値を取得する．
    function get_minimum_coord_value(this) result(min_coord_val)
        implicit none

        class(axis), intent(in) :: this
            !! 当該実体仮引数

        real(real64) :: min_coord_val

        min_coord_val = this%min
    end function get_minimum_coord_value

    !| 軸の最大値を設定する．
    subroutine set_maximum_coord_value(this, min_coord_val)
        implicit none
        !&<
        class(axis) , intent(inout) :: this
            !! 当該実体仮引数
        real(real64), intent(in)    :: min_coord_val
            !! 軸の最小値
        !&>

        this%max = min_coord_val
    end subroutine set_maximum_coord_value

    !| 軸の最大値を取得する．
    function get_maximum_coord_value(this) result(max_coord_val)
        implicit none

        class(axis), intent(in) :: this
            !! 当該実体仮引数

        real(real64) :: max_coord_val

        max_coord_val = this%max
    end function get_maximum_coord_value
end module coordinate_axis

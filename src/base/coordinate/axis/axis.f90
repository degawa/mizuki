!| 座標系の各軸の情報（最小値と最大値）を取り扱うモジュール．
module coordinate_axis
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    implicit none
    private
    public :: axis_min_idx, axis_max_idx

    enum, bind(c)
        enumerator :: axis_min_idx = 1
            !! 軸の最小値を参照するための配列添字
        enumerator :: axis_max_idx
            !! 軸の最大値を参照するための配列添字
    end enum

    type, public :: axis
        real(real64), private :: min = 0d0
            !! 軸の最小値
        real(real64), private :: max = 0d0
            !! 軸の最大値
    contains
        procedure, public, pass :: set_coord_values
            !! 軸の最小値と最大値を設定
        procedure, public, pass :: get_coord_values
            !! 軸の最小値と最大値を取得
        procedure, public, pass :: set_minimum_coord_value
            !! 軸の最小値を取得
        procedure, public, pass :: get_minimum_coord_value
            !! 軸の最小値を取得
        procedure, public, pass :: set_maximum_coord_value
            !! 軸の最大値を設定
        procedure, public, pass :: get_maximum_coord_value
            !! 軸の最大値を取得
        procedure, public, pass :: get_length
            !! 軸の長さを取得
        procedure, public, pass :: assign_array
            !! 軸の最小値と最大値を持った配列を代入
        procedure, public, pass :: assign_axis
            !! `axis`型の変数の値をコピー
        generic :: assignment(=) => assign_array, assign_axis
    end type axis

contains

    !| 軸の最小値と最大値を持った配列を代入する．
    subroutine assign_array(this, ref_array)
        implicit none
        !&<
        class(axis) , intent(inout) :: this
            !! 当該実体仮引数
        real(real64), intent(in)    :: ref_array(2)
            !! 軸の最小値と最大値<br> `[min, max]`の順に格納
        !&>

        this%min = ref_array(axis_min_idx)
        this%max = ref_array(axis_max_idx)
    end subroutine assign_array

    !| `axis`型の変数の値をコピーする．
    subroutine assign_axis(this, ref_axis)
        implicit none
        !&<
        class(axis), intent(inout) :: this
            !! 当該実体仮引数
        type(axis), intent(in) :: ref_axis
            !! コピー元の`axis`型変数
        !&>

        this%min = ref_axis%min
        this%max = ref_axis%max
    end subroutine assign_axis

    !------------------------------------------------------------------!
    !| 軸の最小値と最大値を設定する．
    subroutine set_coord_values(this, coord_vals)
        implicit none
        !&<
        class(axis) , intent(inout) :: this
            !! 当該実体仮引数
        real(real64), intent(in)    :: coord_vals(2)
            !! 軸の最小値と最大値<br> `[min, max]`の順に格納
        !&>

        !|@todo
        ! coord_vals(min_coord) > coord_vals(max_coord)
        ! の場合のテストの作成
        !@endtodo

        this = coord_vals
    end subroutine set_coord_values

    !| 軸の最小値と最大値を取得する．
    function get_coord_values(this) result(coord_vals)
        implicit none

        class(axis), intent(in) :: this
            !! 当該実体仮引数

        real(real64) :: coord_vals(2)
            !! 軸の最小値と最大値<br> `[min, max]`の順に格納

        coord_vals(axis_min_idx) = this%min
        coord_vals(axis_max_idx) = this%max
    end function get_coord_values

    !------------------------------------------------------------------!
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

    !------------------------------------------------------------------!
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

    !------------------------------------------------------------------!
    !| 軸の長さを取得する．
    function get_length(this) result(length)
        implicit none

        class(axis), intent(in) :: this
            !! 当該実体仮引数

        real(real64) :: length

        length = this%max - this%min
    end function get_length
end module coordinate_axis

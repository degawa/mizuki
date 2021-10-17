!| デカルト座標系を表す派生型を定義したモジュール．
module coordinate_Cartesian
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    use :: coordinate_axis, only:axis
    implicit none
    private
    public :: Cartesian_x_dir_idx, Cartesian_y_dir_idx, Cartesian_z_dir_idx
    public :: Cartesian_x_min_idx, Cartesian_x_max_idx, &
              Cartesian_y_min_idx, Cartesian_y_max_idx, &
              Cartesian_z_min_idx, Cartesian_z_max_idx

    enum, bind(c)
        enumerator :: Cartesian_x_dir_idx = 1
            !! デカルト座標系の軸方向成分\(x\)を参照するための配列添字
        enumerator :: Cartesian_y_dir_idx
            !! デカルト座標系の軸方向成分\(y\)を参照するための配列添字
        enumerator :: Cartesian_z_dir_idx
            !! デカルト座標系の軸方向成分\(z\)を参照するための配列添字
    end enum

    enum, bind(c)
        enumerator :: Cartesian_x_min_idx = 1
            !! デカルト座標系の\(x\)軸の最小値を参照するための配列添字
        enumerator :: Cartesian_x_max_idx
            !! デカルト座標系の\(x\)軸の最大値を参照するための配列添字
        enumerator :: Cartesian_y_min_idx
            !! デカルト座標系の\(y\)軸の最小値を参照するための配列添字
        enumerator :: Cartesian_y_max_idx
            !! デカルト座標系の\(y\)軸の最大値を参照するための配列添字
        enumerator :: Cartesian_z_min_idx
            !! デカルト座標系の\(z\)軸の最小値を参照するための配列添字
        enumerator :: Cartesian_z_max_idx
            !! デカルト座標系の\(z\)軸の最大値を参照するための配列添字
    end enum

    !| 1次元デカルト座標系
    type, public :: Cartesian_coordinate_1d
        type(axis), public :: x
            !! \(x\)座標
    contains
        procedure, public, pass :: set_coordinate_1d_from_array
            !! 配列の値に基づいて座標系の各軸を設定
        procedure, public, pass :: set_coodinate_1d_from_axis
            !! axis型変数に基づいて座標系の各軸を設定
        !&<
        generic :: construct => set_coordinate_1d_from_array, &
                                set_coodinate_1d_from_axis
        !&>

        procedure, public, pass :: get_coordinate => get_coordinate_1d
            !! 座標系の各軸の最小値と最大値を配列で返す<br>
            !! `=[x_min, x_max]`
    end type Cartesian_coordinate_1d

    ! getterの実装が煩雑になり，それが継承する恩恵を打ち消すので，
    ! Cartesian_coodinate_1d型を**継承しない**

    !| 2次元デカルト座標系<br>
    type, public :: Cartesian_coordinate_2d
        type(axis), public :: x
            !! \(x\)座標
        type(axis), public :: y
            !! \(y\)座標
    contains
        procedure, public, pass :: set_coordinate_2d_from_array
            !! 配列の値に基づいて座標系の各軸を設定
        procedure, public, pass :: set_coodinate_2d_from_axis
            !! axis型変数に基づいて座標系の各軸を設定
        !&<
        generic :: construct => set_coordinate_2d_from_array, &
                                set_coodinate_2d_from_axis
        !&>

        procedure, public, pass :: get_coordinate => get_coordinate_2d
            !! 座標系の各軸の最小値と最大値を配列で返す<br>
            !! `=[x_min, x_max, y_min, y_max]`
    end type Cartesian_coordinate_2d

    !| 3次元デカルト座標系
    type, public :: Cartesian_coordinate_3d
        type(axis), public :: x
            !! \(x\)座標
        type(axis), public :: y
            !! \(y\)座標
        type(axis), public :: z
            !! \(z\)座標
    contains
        procedure, public, pass :: set_coordinate_3d_from_array
            !! 配列の値に基づいて座標系の各軸を設定
        procedure, public, pass :: set_coodinate_3d_from_axis
            !! axis型変数に基づいて座標系の各軸を設定
        !&<
        generic :: construct => set_coordinate_3d_from_array, &
                                set_coodinate_3d_from_axis
        !&>
        procedure, public, pass :: get_coordinate => get_coordinate_3d
            !! 座標系の各軸の最小値と最大値を配列で返す<br>
            !! `=[x_min, x_max, y_min, y_max, z_min, z_max]`
    end type Cartesian_coordinate_3d

contains
    !------------------------------------------------------------------!
    !| 配列の値に基づいて1次元デカルト座標系型変数を設定する．
    subroutine set_coordinate_1d_from_array(this, x_coord_vals)
        implicit none
        !&<
        class(Cartesian_coordinate_1d)  , intent(inout) :: this
            !! 2次元デカルト座標型の当該実体仮引数
        real(real64)                    , intent(in)    :: x_coord_vals(2)
            !! \(x\)軸の最小値と最大値<br> `[min, max]`の順に格納
        !&>

        this%x = x_coord_vals(:)
    end subroutine set_coordinate_1d_from_array

    !| 軸の値に基づいて1次元デカルト座標系型変数を設定する．
    subroutine set_coodinate_1d_from_axis(this, x_axis)
        implicit none
        !&<
        class(Cartesian_coordinate_1d)  , intent(inout) :: this
            !! 2次元デカルト座標型の当該実体仮引数
        type(axis)                      , intent(in)    :: x_axis
            !! axis型で表された\(x\)軸
        !&>

        this%x = x_axis
    end subroutine set_coodinate_1d_from_axis

    !| 1次元座標系のx軸の座標値を取得する．
    function get_coordinate_1d(this) result(coord_vals)
        implicit none
        class(Cartesian_coordinate_1d), intent(in) :: this
            !! 2次元デカルト座標型の当該実体仮引数

        real(real64) :: coord_vals(2*1)
            !! 各軸の座標値`=[x_min, x_max]`

        coord_vals = [this%x%get_coord_values()]
    end function get_coordinate_1d

    !------------------------------------------------------------------!
    !| 配列の値に基づいて2次元デカルト座標系型変数を設定する．
    subroutine set_coordinate_2d_from_array(this, x_coord_vals, y_coord_vals)
        implicit none
        !&<
        class(Cartesian_coordinate_2d)  , intent(inout) :: this
            !! 2次元デカルト座標型の当該実体仮引数
        real(real64)                    , intent(in)    :: x_coord_vals(2)
            !! \(x\)軸の最小値と最大値<br> `[min, max]`の順に格納
        real(real64)                    , intent(in)    :: y_coord_vals(2)
            !! \(y\)軸の最小値と最大値<br> `[min, max]`の順に格納
        !&>

        this%x = x_coord_vals(:)
        this%y = y_coord_vals(:)
    end subroutine set_coordinate_2d_from_array

    !| 軸の値に基づいて2次元デカルト座標系型変数を設定する．
    subroutine set_coodinate_2d_from_axis(this, x_axis, y_axis)
        implicit none
        !&<
        class(Cartesian_coordinate_2d)  , intent(inout) :: this
            !! 2次元デカルト座標型の当該実体仮引数
        type(axis)                      , intent(in)    :: x_axis
            !! axis型で表された\(x\)軸
        type(axis)                      , intent(in)    :: y_axis
            !! axis型で表された\(y\)軸
        !&>

        this%x = x_axis
        this%y = y_axis
    end subroutine set_coodinate_2d_from_axis

    !| 2次元デカルト座標系の各軸の座標値を取得する．
    function get_coordinate_2d(this) result(coord_vals)
        implicit none
        class(Cartesian_coordinate_2d), intent(in) :: this
            !! 2次元デカルト座標型の当該実体仮引数

        real(real64) :: coord_vals(2*2)
            !! 各軸の座標値`=[x_min, x_max, y_min, y_max]`

        coord_vals = [this%x%get_coord_values(), &
                      this%y%get_coord_values()]
    end function get_coordinate_2d

    !------------------------------------------------------------------!
    !| 配列の値に基づいて3次元デカルト座標系型変数を設定する．
    subroutine set_coordinate_3d_from_array(this, x_coord_vals, y_coord_vals, z_coord_vals)
        implicit none
        !&<
        class(Cartesian_coordinate_3d)  , intent(inout) :: this
            !! 2次元デカルト座標型の当該実体仮引数
        real(real64)                    , intent(in)    :: x_coord_vals(2)
            !! \(x\)軸の最小値と最大値<br> `[min, max]`の順に格納
        real(real64)                    , intent(in)    :: y_coord_vals(2)
            !! \(y\)軸の最小値と最大値<br> `[min, max]`の順に格納
        real(real64)                    , intent(in)    :: z_coord_vals(2)
            !! \(z\)軸の最小値と最大値<br> `[min, max]`の順に格納
        !&>

        this%x = x_coord_vals(:)
        this%y = y_coord_vals(:)
        this%z = z_coord_vals(:)
    end subroutine set_coordinate_3d_from_array

    !| 軸の値に基づいて3次元デカルト座標系型変数を設定する．
    subroutine set_coodinate_3d_from_axis(this, x_axis, y_axis, z_axis)
        implicit none
        !&<
        class(Cartesian_coordinate_3d)  , intent(inout) :: this
            !! 2次元デカルト座標型の当該実体仮引数
        type(axis)                      , intent(in)    :: x_axis
            !! axis型で表された\(x\)軸
        type(axis)                      , intent(in)    :: y_axis
            !! axis型で表された\(y\)軸
        type(axis)                      , intent(in)    :: z_axis
            !! axis型で表された\(z\)軸
        !&>

        this%x = x_axis
        this%y = y_axis
        this%z = z_axis
    end subroutine set_coodinate_3d_from_axis

    !| 3次元デカルト座標系の各軸の座標値を取得する．
    function get_coordinate_3d(this) result(coord_vals)
        implicit none
        class(Cartesian_coordinate_3d), intent(in) :: this
            !! 2次元デカルト座標型の当該実体仮引数

        real(real64) :: coord_vals(2*3)
            !! 各軸の座標値`=[x_min, x_max, y_min, y_max]`

        coord_vals = [this%x%get_coord_values(), &
                      this%y%get_coord_values(), &
                      this%z%get_coord_values()]
    end function get_coordinate_3d
end module coordinate_Cartesian

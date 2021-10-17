!| 配列の添字の範囲を取り扱う型を定義したモジュール．
module arrayRange
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    implicit none
    private
    public :: get_array1d_range_from_bounds
    public :: get_array2d_range_from_bounds
    public :: get_array3d_range_from_bounds
    public :: get_array1d_range_as_array
    public :: get_array2d_range_as_array
    public :: get_array3d_range_as_array
    public :: array1d_range_rank1_min_idx, array1d_range_rank1_max_idx
    public :: array2d_range_rank1_min_idx, array2d_range_rank2_min_idx, &
              array2d_range_rank1_max_idx, array2d_range_rank2_max_idx
    public :: array3d_range_rank1_min_idx, array3d_range_rank2_min_idx, array3d_range_rank3_min_idx, &
              array3d_range_rank1_max_idx, array3d_range_rank2_max_idx, array3d_range_rank3_max_idx

    !| 配列の上下限を保持する派生型
    type, public :: array_range
        integer(int32), public :: lower = 0
            !! 配列の下限値
        integer(int32), public :: upper = 0
            !! 配列の上限値
    end type array_range

    !| 1次元配列の添字範囲を取り扱う型
    type, public :: array1d_range
        type(array_range), public :: rank(1)
            !! rank 1 の配列上下限
    end type array1d_range

    !| 2次元配列の添字範囲を取り扱う型
    type, public :: array2d_range
        type(array_range), public :: rank(2)
            !! rank 1, 2 の配列上下限
    end type array2d_range

    !| 3次元配列の添字範囲を取り扱う型
    type, public :: array3d_range
        type(array_range), public :: rank(3)
            !! rank 1, 2, 3 の配列上下限
    end type array3d_range

    enum, bind(c)
        enumerator :: array1d_range_rank1_min_idx = 1
        enumerator :: array1d_range_rank1_max_idx
    end enum

    enum, bind(c)
        enumerator :: array2d_range_rank1_min_idx = 1
        enumerator :: array2d_range_rank2_min_idx
        enumerator :: array2d_range_rank1_max_idx
        enumerator :: array2d_range_rank2_max_idx
    end enum

    enum, bind(c)
        enumerator :: array3d_range_rank1_min_idx = 1
        enumerator :: array3d_range_rank2_min_idx
        enumerator :: array3d_range_rank3_min_idx
        enumerator :: array3d_range_rank1_max_idx
        enumerator :: array3d_range_rank2_max_idx
        enumerator :: array3d_range_rank3_max_idx
    end enum
contains
    !------------------------------------------------------------------!
    !| 1次元配列の上下限値を引数で取り，`array1d_range`型の変数を返す
    function get_array1d_range_from_bounds(bounds) result(new_range)
        implicit none
        integer(int32), parameter :: dim = 1
        integer(int32), intent(in) :: bounds(dim*2)
            !! 1次元配列の上下限値 `=[rank 1 lower, rank 1 upper]`
            !!
            !!### Example
            !!```Fortran
            !!real(real64),allocatable :: array(:)
            !!type(array1d_range) :: range
            !!...
            !!range = get_array1d_range_from_bounds(lbound(array), ubound(array))
            !!```

        type(array1d_range) :: new_range
            !! 戻り値
        !&<
        new_range%rank(:)%lower = bounds(    1:dim  )
        new_range%rank(:)%upper = bounds(dim+1:dim*2)
        !&>
    end function get_array1d_range_from_bounds

    !| `array1d_range`型の引数から，配列の上下限値を配列で返す
    function get_array1d_range_as_array(range) result(new_range)
        implicit none

        type(array1d_range), intent(in) :: range
            !! 1次元配列の上下限値

        integer(int32), parameter :: dim = 1
        integer(int32) :: new_range(dim*2)
            !! 戻り値 `=[rank 1 lower, rank1 upper]`
        !&<
        new_range(    1:dim  ) = range%rank(:)%lower
        new_range(dim+1:dim*2) = range%rank(:)%upper
        !&>
    end function get_array1d_range_as_array

    !| 2次元配列の上下限値を引数で取り，`array2d_range`型の変数を返す
    function get_array2d_range_from_bounds(bounds) result(new_range)
        implicit none
        integer(int32), parameter :: dim = 2
        integer(int32), intent(in) :: bounds(dim*2)
            !! 2次元配列の上下限値 `=[rank 1 lower, rank 2 lower, rank 1 upper, rank 2 upper]`
            !!
            !!### Example
            !!```Fortran
            !!real(real64),allocatable :: array(:,:)
            !!type(array2d_range) :: range
            !!...
            !!range = get_array2d_range_from_bounds(lbound(array), ubound(array))
            !!```

        type(array2d_range) :: new_range
            !! 戻り値
        !&<
        new_range%rank(:)%lower = bounds(    1:dim  )
        new_range%rank(:)%upper = bounds(dim+1:dim*2)
        !&>
    end function get_array2d_range_from_bounds

    !| `array2d_range`型の引数から，配列の上下限値を配列で返す
    function get_array2d_range_as_array(range) result(new_range)
        implicit none
        type(array2d_range), intent(in) :: range
            !! 2次元配列の上下限値

        integer(int32), parameter :: dim = 2
        integer(int32) :: new_range(dim*2)
            !! 戻り値 `=[rank 1 lower, rank 2 lower, rank 1 upper, rank 2 upper]`

        !&<
        new_range(    1:dim  ) = range%rank(:)%lower
        new_range(dim+1:dim*2) = range%rank(:)%upper
        !&>
    end function get_array2d_range_as_array

    !| 3次元配列の上下限値を引数で取り，`array3d_range`型の変数を返す
    function get_array3d_range_from_bounds(bounds) result(new_range)
        implicit none
        integer(int32), parameter :: dim = 3
        integer(int32), intent(in) :: bounds(dim*2)
            !! 2次元配列の上下限値 `=[rank 1 lower, rank 2 lower, rank 3 lower, rank 1 upper, rank 2 upper, rank 3 upper]`
            !!
            !!### Example
            !!```Fortran
            !!real(real64),allocatable :: array(:,:,:)
            !!type(array3d_range) :: range
            !!...
            !!range = get_array3d_range_from_bounds(lbound(array), ubound(array))
            !!```

        type(array3d_range) :: new_range
            !! 戻り値
        !&<
        new_range%rank(:)%lower = bounds(    1:dim  )
        new_range%rank(:)%upper = bounds(dim+1:dim*2)
        !&>
    end function get_array3d_range_from_bounds

    !| `array3d_range`型の引数から，配列の上下限値を配列で返す
    function get_array3d_range_as_array(range) result(new_range)
        implicit none
        type(array3d_range), intent(in) :: range
            !! 2次元配列の上下限値

        integer(int32), parameter :: dim = 3
        integer(int32) :: new_range(dim*2)
            !! 戻り値 `=[rank 1 lower, rank 2 lower, rank 3 lower, rank 1 upper, rank 2 upper, rank 3 upper]`

        !&<
        new_range(    1:dim  ) = range%rank(:)%lower
        new_range(dim+1:dim*2) = range%rank(:)%upper
        !&>
    end function get_array3d_range_as_array
end module arrayRange

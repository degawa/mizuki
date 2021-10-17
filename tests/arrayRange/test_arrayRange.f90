program test_arrayRange
    use, intrinsic :: iso_fortran_env
    use :: coordinate_Cartesian
    use :: arrayRange
    use :: assertion
    implicit none

    character(*), parameter :: test_name = "array range"//" "

    call test_array1d_range()
    call test_array2d_range()
    call test_array3d_range()

contains
    subroutine test_array1d_range()
        implicit none

        type(array1d_range) :: range
        real(real64) :: array(-2:10)
        integer(int32) :: bounds(2)

        range = get_array1d_range_from_bounds([lbound(array), ubound(array)])
        call assert_equal([range%rank(1)%lower, range%rank(1)%upper], [-2, 10], &
                          test_name//"get_array1d_range_from_bounds")

        range%rank(1)%lower = -10
        range%rank(1)%upper = 2
        call assert_equal([range%rank(1)%lower, range%rank(1)%upper], [-10, 2], &
                          test_name//"get_array1d_range_as_array prepare")

        bounds = get_array1d_range_as_array(range)
        call assert_equal(bounds, [-10, 2], &
                          test_name//"get_array1d_range_as_array")

        call assert_equal(bounds(array1d_range_rank1_min_idx), -10, &
                          test_name//"array1d_range_rank1_min_idx")
        call assert_equal(bounds(array1d_range_rank1_max_idx), 2, &
                          test_name//"array1d_range_rank1_max_idx")
    end subroutine test_array1d_range

    subroutine test_array2d_range()
        implicit none

        type(array2d_range) :: range
        real(real64) :: array(-5:5, 1:10)
        integer(int32) :: bounds(4)

        range = get_array2d_range_from_bounds([lbound(array), ubound(array)])
        call assert_equal([range%rank(1)%lower, range%rank(2)%lower, range%rank(1)%upper, range%rank(2)%upper], [-5, 1, 5, 10], &
                          test_name//"get_array2d_range_from_bounds")

        range%rank(1)%lower = 1
        range%rank(1)%upper = 10
        range%rank(2)%lower = -5
        range%rank(2)%upper = 5
        call assert_equal([range%rank(1)%lower, range%rank(2)%lower, range%rank(1)%upper, range%rank(2)%upper], [1, -5, 10, 5], &
                          test_name//"get_array2d_range_as_array prepare")

        bounds = get_array2d_range_as_array(range)
        call assert_equal(bounds, [1, -5, 10, 5], &
                          test_name//"get_array2d_range_as_array")

        call assert_equal(bounds(array2d_range_rank1_min_idx), 1, &
                          test_name//"array2d_range_rank1_min_idx")
        call assert_equal(bounds(array2d_range_rank1_max_idx), 10, &
                          test_name//"array2d_range_rank1_max_idx")
        call assert_equal(bounds(array2d_range_rank2_min_idx), -5, &
                          test_name//"array2d_range_rank2_min_idx")
        call assert_equal(bounds(array2d_range_rank2_max_idx), 5, &
                          test_name//"array2d_range_rank2_max_idx")
    end subroutine test_array2d_range

    subroutine test_array3d_range()
        implicit none

        type(array3d_range) :: range
        real(real64) :: array(10:20, -1:1, -5:-4)
        integer(int32) :: bounds(6)

        range = get_array3d_range_from_bounds([lbound(array), ubound(array)])
        call assert_equal([range%rank(1)%lower, range%rank(2)%lower, range%rank(3)%lower, &
                           range%rank(1)%upper, range%rank(2)%upper, range%rank(3)%upper], [10, -1, -5, 20, 1, -4], &
                          test_name//"get_array3d_range_from_bounds")

        range%rank(1)%lower = -1
        range%rank(1)%upper = 1
        range%rank(2)%lower = -5
        range%rank(2)%upper = -4
        range%rank(3)%lower = 10
        range%rank(3)%upper = 20
        call assert_equal([range%rank(1)%lower, range%rank(2)%lower, range%rank(3)%lower, &
                           range%rank(1)%upper, range%rank(2)%upper, range%rank(3)%upper], [-1, -5, 10, 1, -4, 20], &
                          test_name//"get_array3d_range_as_array prepare")

        bounds = get_array3d_range_as_array(range)
        call assert_equal(bounds, [-1, -5, 10, 1, -4, 20], &
                          test_name//"get_array3d_range_as_array")

        call assert_equal(bounds(array3d_range_rank1_min_idx), -1, &
                          test_name//"array3d_range_rank1_min_idx")
        call assert_equal(bounds(array3d_range_rank1_max_idx), 1, &
                          test_name//"array3d_range_rank1_max_idx")
        call assert_equal(bounds(array3d_range_rank2_min_idx), -5, &
                          test_name//"array3d_range_rank2_min_idx")
        call assert_equal(bounds(array3d_range_rank2_max_idx), -4, &
                          test_name//"array3d_range_rank2_max_idx")
        call assert_equal(bounds(array3d_range_rank3_min_idx), 10, &
                          test_name//"array3d_range_rank3_min_idx")
        call assert_equal(bounds(array3d_range_rank3_max_idx), 20, &
                          test_name//"array3d_range_rank3_max_idx")
    end subroutine test_array3d_range

end program test_arrayRange

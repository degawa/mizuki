program test_axis
    use, intrinsic :: iso_fortran_env
    use :: coordinate_axis
    use :: coordinate_axis_factory
    use :: assertion
    implicit none

    character(*), parameter :: test_name = "axis "

    call test_get_coord_values()
    call test_set_coord_values()
    call test_factory()

contains
    subroutine test_get_coord_values()
        implicit none

        type(axis) :: an_axis
        real(real64) :: min_coord = -1d0, max_coord = -1d0
        real(real64) :: coord_val(2) = [-1d0, -1d0]

        min_coord = an_axis%get_minimum_coord_value()
        max_coord = an_axis%get_maximum_coord_value()

        call assert_equal(min_coord, 0d0, test_name//"get_min_coord_val")
        call assert_equal(max_coord, 0d0, test_name//"get_max_coord_val")

        coord_val(:) = an_axis%get_coord_values()
        call assert_equal(coord_val, [0d0, 0d0], test_name//"get_coord_values")
    end subroutine test_get_coord_values

    subroutine test_set_coord_values()
        implicit none

        type(axis) :: an_axis
        real(real64) :: coord_val(2) = [-1d0, -1d0]

        call an_axis%set_coord_values([-10d0, 10d0])
        coord_val(:) = an_axis%get_coord_values()
        call assert_equal(coord_val, [-10d0, 10d0], test_name//"set_coord_values")

        call an_axis%set_maximum_coord_value(5d0)
        call an_axis%set_minimum_coord_value(-5d0)
        coord_val(:) = an_axis%get_coord_values()

        call assert_equal(coord_val, [-5d0, 5d0], test_name//"set maximum and minimum coord value")
    end subroutine test_set_coord_values

    subroutine test_factory()
        implicit none

        type(axis) :: an_axis
        type(axis), allocatable :: axis_allc
        real(real64) :: coord_val(2)

        call an_axis%set_coord_values([-10d0, 10d0])

        ! factoryをコンストラクタの代わりに使う
        an_axis = axis_factory(-1d0, 1d0)
        coord_val(:) = an_axis%get_coord_values()
        call assert_equal(coord_val, [-1d0, 1d0], test_name//"factory")

        ! allocateのsourceにfactoryの戻り値を指定
        ! 最小値，最大値を指定
        allocate (axis_allc, source=axis_factory(-2d0, 2d0))
        coord_val(:) = axis_allc%get_coord_values()
        call assert_equal(coord_val, [-2d0, 2d0], test_name//"value-based-factory-sourced allocation")
        deallocate (axis_allc)

        ! 配列で最小値と最大値を指定
        allocate (axis_allc, source=axis_factory([-4d0, 4d0]))
        coord_val(:) = axis_allc%get_coord_values()
        call assert_equal(coord_val, [-4d0, 4d0], test_name//"array-based-factory-sourced allocation")
        deallocate (axis_allc)

    end subroutine test_factory
end program test_axis

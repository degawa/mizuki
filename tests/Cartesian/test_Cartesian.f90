program test_axis
    use, intrinsic :: iso_fortran_env
    use :: coordinate_axis_factory
    use :: coordinate_Cartesian
    use :: assertion
    implicit none

    character(*), parameter :: test_name = "Cartesian"//" "

    call test_index()
    call test_Cartesian_coordinate_1d()
    call test_Cartesian_coordinate_2d()
    call test_Cartesian_coordinate_3d()

contains
    subroutine test_index()
        implicit none

        call assert_equal(Cartesian_x_dir_idx, 1, test_name//"array index to refer x direction")
        call assert_equal(Cartesian_y_dir_idx, 2, test_name//"array index to refer y direction")
        call assert_equal(Cartesian_z_dir_idx, 3, test_name//"array index to refer z direction")

        call assert_equal(Cartesian_x_min_idx, 1, test_name//"array index to refer minimum value in  x direction")
        call assert_equal(Cartesian_x_max_idx, 2, test_name//"array index to refer maximum value in  x direction")
        call assert_equal(Cartesian_y_min_idx, 3, test_name//"array index to refer minimum value in  y direction")
        call assert_equal(Cartesian_y_max_idx, 4, test_name//"array index to refer maximum value in  y direction")
        call assert_equal(Cartesian_z_min_idx, 5, test_name//"array index to refer minimum value in  z direction")
        call assert_equal(Cartesian_z_max_idx, 6, test_name//"array index to refer maximum value in  z direction")

    end subroutine test_index

    subroutine test_Cartesian_coordinate_1d()
        implicit none

        type(Cartesian_coordinate_1d) :: coord_syst

        call coord_syst%construct(x_coord_vals=[-10d0, 10d0])
        call assert_equal(coord_syst%get_coordinate(), [-10d0, 10d0], &
                          test_name//"Cartesian 1d construct from array")

        call coord_syst%construct(x_axis=axis_factory([-4d0, 4d0]))
        call assert_equal(coord_syst%get_coordinate(), [-4d0, 4d0], &
                          test_name//"Cartesian 1d construct from axis")
    end subroutine test_Cartesian_coordinate_1d

    subroutine test_Cartesian_coordinate_2d()
        implicit none

        type(Cartesian_coordinate_2d) :: coord_syst

        call coord_syst%construct(x_coord_vals=[-2d0, 2d0], y_coord_vals=[-1d0, 1d0])
        call assert_equal(coord_syst%get_coordinate(), [-2d0, 2d0, -1d0, 1d0], &
                          test_name//"Cartesian 2d construct from array")

        call coord_syst%construct(x_axis=axis_factory([-4d0, 4d0]), y_axis=axis_factory([-3d0, 3d0]))
        call assert_equal(coord_syst%get_coordinate(), [-4d0, 4d0, -3d0, 3d0], &
                          test_name//"Cartesian 2d construct from axis")
    end subroutine test_Cartesian_coordinate_2d

    subroutine test_Cartesian_coordinate_3d()
        implicit none

        type(Cartesian_coordinate_3d) :: coord_syst

        call coord_syst%construct(x_coord_vals=[-3d0, -2d0], y_coord_vals=[-1d0, 0d0], z_coord_vals=[1d0, 2d0])
        call assert_equal(coord_syst%get_coordinate(), [-3d0, -2d0, -1d0, 0d0, 1d0, 2d0], &
                          test_name//"Cartesian 3d construct from array")

        call coord_syst%construct(x_axis=axis_factory([-6d0, -5d0]), &
                                  y_axis=axis_factory([-4d0, -3d0]), &
                                  z_axis=axis_factory([-2d0, -1d0]))
        call assert_equal(coord_syst%get_coordinate(), [-6d0, -5d0, -4d0, -3d0, -2d0, -1d0], &
                          test_name//"Cartesian 3d construct from axis")
    end subroutine test_Cartesian_coordinate_3d
end program test_axis

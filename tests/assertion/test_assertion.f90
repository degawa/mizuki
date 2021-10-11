program test_assertion
    use, intrinsic :: iso_fortran_env
    use :: assertion_common_status
    implicit none

    character(*), parameter :: correct_result = "<- printing FAILED is not a problem"

    call test_common_status()
    call test_common_checkTrue()
    call test_common_optval()
    call test_common_setTolerance()
    call test_common_stopOnFailure()
    call test_sameShape_compareArrayShape()
    call test_sameShape_expect()
    call test_logical_expect()
    call test_equal_compareArrayValue()
    call test_equal_expect()

contains
    !| 条件の成否し，不成立ならプログラムを止めるための手続
    subroutine check(condition, test_name)
        implicit none

        logical, intent(in) :: condition
        character(*), intent(in) :: test_name

        if (condition) then
            write (output_unit, '(A)') "PASSED "//test_name
        else
            write (output_unit, '(A)') "FAILED "//test_name
            error stop
        end if
    end subroutine check

    !| subroutine statusのテスト
    subroutine test_common_status()
        implicit none

        character(*), parameter :: test_name = "assert common status "

        call check(success .eqv. .true., test_name//"success")

        call check(failure .eqv. .false., test_name//"failure")
    end subroutine test_common_status

    !| subroutine check_trueのテスト
    subroutine test_common_checkTrue()
        use :: assertion_common_checkTrue
        implicit none

        character(*), parameter :: test_name = "assert common check_true "
        logical :: stat

        call check_true(.true., "", stat)
        call check(stat .eqv. success, test_name//"success")

        call check_true(.false., correct_result, stat)
        call check(stat .eqv. failure, test_name//"failure")
    end subroutine test_common_checkTrue

    !| function optvalのテスト
    subroutine test_common_optval()
        use :: assertion_common_optval
        implicit none

        block
            character(*), parameter :: test_name = "assert common optval_logical "
            call check(optval(.true., .false.) .eqv. .true., test_name//"x=.true.")
            call check(optval(.false., .true.) .eqv. .false., test_name//"x=.false.")
            call check(optval(default=.true.) .eqv. .true., test_name//"default=.true.")
            call check(optval(default=.false.) .eqv. .false., test_name//"default=.false.")
        end block

        block
            character(*), parameter :: test_name = "assert common optval_real32 "
            call check(abs(optval(1e0, 0e0) - 1e0) < epsilon(0e0), test_name//" x not zero")
            call check(abs(optval(0e0, 1e0) - 0e0) < epsilon(0e0), test_name//"x zero")
            call check(abs(optval(default=1e0) - 1e0) < epsilon(0e0), test_name//"default not zero")
        end block

        block
            character(*), parameter :: test_name = "assert common optval_real64 "
            call check(abs(optval(1d0, 0d0) - 1d0) < epsilon(0d0), test_name//"x not zero")
            call check(abs(optval(0d0, 1d0) - 0d0) < epsilon(0d0), test_name//"x zero")
            call check(abs(optval(default=1d0) - 1d0) < epsilon(0d0), test_name//"default not zero")
        end block
    end subroutine test_common_optval

    !| function set_toleranceのテスト
    subroutine test_common_setTolerance()
        use :: assertion_common_setTolerance
        implicit none

        block
            character(*), parameter :: test_name = "assert common set_tolerance_real32 "
            call check(abs(set_tolerance(1e0, 0e0) - 1e0) < epsilon(0e0), test_name//"x not zero")
            call check(abs(set_tolerance(0e0, 1e0) - 0e0) < epsilon(0e0), test_name//"x zero")
            call check(abs(set_tolerance(default=1e0) - 1e0) < epsilon(0e0), test_name//"default not zero")
        end block

        block
            character(*), parameter :: test_name = "assert common set_tolerance_real64 "
            call check(abs(set_tolerance(1d0, 0d0) - 1d0) < epsilon(0d0), test_name//"x not zero")
            call check(abs(set_tolerance(0d0, 1d0) - 0d0) < epsilon(0d0), test_name//"x zero")
            call check(abs(set_tolerance(default=1d0) - 1d0) < epsilon(0d0), test_name//"default not zero")
        end block
    end subroutine test_common_setTolerance

    !| subroutine stop_on_failureのテスト<br>
    ! 成功時に止まらないことだけを確認
    subroutine test_common_stopOnFailure()
        use :: assertion_common_stopOnFailure
        implicit none

        call stop_on_failure(success)
        ! call stop_on_failure(failure)
    end subroutine test_common_stopOnFailure

    !| function compareArrayShapeのテスト
    subroutine test_sameShape_compareArrayShape()
        use :: assertion_sameShape_compareArrayShape, only:are_same_shape
        implicit none

        character(*), parameter :: test_name = "assert same shape are_same_shape"

        block
            call check(are_same_shape([integer(int32) :: 1, 2, 3], &
                                      [integer(int32) :: 4, 5, 6]), test_name//"int32 rank 1 success")

            call check(.not. are_same_shape([integer(int32) :: 1, 2, 3], &
                                            [integer(int32) :: 1, 2]), test_name//"int32 rank 1 failure")

            call check(are_same_shape(reshape([integer(int32) :: 1, 2, 3, 4, 5, 6], [3, 2]), &
                                      reshape([integer(int32) :: 4, 5, 6, 7, 8, 9], [3, 2])), &
                       test_name//"int32 rank 2 success")
            call check(.not. are_same_shape(reshape([integer(int32) :: 1, 2, 3, 4, 5, 6], [3, 2]), &
                                            reshape([integer(int32) :: 1, 2, 3, 4, 5, 6], [2, 3])), &
                       test_name//"int32 rank 2 failure")

            call check(are_same_shape(reshape([integer(int32) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), &
                                      reshape([integer(int32) :: 10, 20, 30, 40, 50, 60, 70, 80, 90], [3, 3])), &
                       test_name//"int32 rank 3 success")
            call check(.not. are_same_shape(reshape([integer(int32) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), &
                                            reshape([integer(int32) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 9])), &
                       test_name//"int32 rank 3 failure")
        end block

        block
            call check(are_same_shape([real(real32) :: 1, 2, 3], &
                                      [real(real32) :: 4, 5, 6]), test_name//"real32 rank 1 success")
            call check(.not. are_same_shape([real(real32) :: 1, 2, 3], &
                                            [real(real32) :: 1, 2]), test_name//"real32 rank 1 failure")

            call check(are_same_shape(reshape([real(real32) :: 1, 2, 3, 4, 5, 6], [3, 2]), &
                                      reshape([real(real32) :: 4, 5, 6, 7, 8, 9], [3, 2])), &
                       test_name//"real32 rank 2 success")
            call check(.not. are_same_shape(reshape([real(real32) :: 1, 2, 3, 4, 5, 6], [3, 2]), &
                                            reshape([real(real32) :: 1, 2, 3, 4, 5, 6], [2, 3])), &
                       test_name//"real32 rank 2 failure")

            call check(are_same_shape(reshape([real(real32) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), &
                                      reshape([real(real32) :: 10, 20, 30, 40, 50, 60, 70, 80, 90], [3, 3])), &
                       test_name//"real32 rank 3 success")
            call check(.not. are_same_shape(reshape([real(real32) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), &
                                            reshape([real(real32) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 9])), &
                       test_name//"real32 rank 3 failure")
        end block

        block
            call check(are_same_shape([real(real64) :: 1, 2, 3], &
                                      [real(real64) :: 4, 5, 6]), test_name//"real64 rank 1 success")
            call check(.not. are_same_shape([real(real64) :: 1, 2, 3], &
                                            [real(real64) :: 1, 2]), test_name//"real64 rank 1 failure")

            call check(are_same_shape(reshape([real(real64) :: 1, 2, 3, 4, 5, 6], [3, 2]), &
                                      reshape([real(real64) :: 4, 5, 6, 7, 8, 9], [3, 2])), &
                       test_name//"real64 rank 2 success")
            call check(.not. are_same_shape(reshape([real(real64) :: 1, 2, 3, 4, 5, 6], [3, 2]), &
                                            reshape([real(real64) :: 1, 2, 3, 4, 5, 6], [2, 3])), &
                       test_name//"real64 rank 2 failure")

            call check(are_same_shape(reshape([real(real64) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), &
                                      reshape([real(real64) :: 10, 20, 30, 40, 50, 60, 70, 80, 90], [3, 3])), &
                       test_name//"real64 rank 3 success")
            call check(.not. are_same_shape(reshape([real(real64) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), &
                                            reshape([real(real64) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 9])), &
                       test_name//"real64 rank 3 failure")
        end block

        block
            call check(are_same_shape(["a", "r", "r", "a", "y"], &
                                      ["b", "s", "s", "b", "z"]), test_name//"char array rank 1 success")
            call check(.not. are_same_shape(["a", "r", "r", "a", "y"], &
                                            ["b", "s", "s", "b"]), test_name//"char array rank 1 failure")
        end block
    end subroutine test_sameShape_compareArrayShape

    !| subroutine expect_same_shapeのテスト
    subroutine test_sameShape_expect()
        use :: assertion_sameShape_expect
        use :: assertion_sameShape_assert
        implicit none
        character(*), parameter :: test_name = "assert same shape expect_same_shape "

        block
            logical :: stat
            call expect_same_shape([integer(int32) :: 1, 2, 3], &
                                   [integer(int32) :: 4, 5, 6], "", stat)
            call check(stat .eqv. success, test_name//"int32 rank 1 success")

            call expect_same_shape([integer(int32) :: 1, 2, 3], &
                                   [integer(int32) :: 4, 5], correct_result, stat)
            call check(stat .eqv. failure, test_name//"int32 rank 1 failure")

            call expect_same_shape(reshape([integer(int32) :: 1, 2, 3, 4, 5, 6], [3, 2]), &
                                   reshape([integer(int32) :: 4, 5, 6, 7, 8, 9], [3, 2]), "", stat)
            call check(stat, test_name//"int32 rank 2 success")

            call expect_same_shape(reshape([integer(int32) :: 1, 2, 3, 4, 5, 6], [3, 2]), &
                                   reshape([integer(int32) :: 1, 2, 3, 4, 5, 6], [2, 3]), correct_result, stat)
            call check(.not. stat, test_name//"int32 rank 2 failure")

            call expect_same_shape(reshape([integer(int32) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), &
                                   reshape([integer(int32) :: 10, 20, 30, 40, 50, 60, 70, 80, 90], [3, 3]), "", stat)
            call check(stat, test_name//"int32 rank 3 success")

            call expect_same_shape(reshape([integer(int32) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), &
                                   reshape([integer(int32) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 9]), correct_result, stat)
            call check(.not. stat, test_name//"int32 rank 3 failure")
        end block

        block
            logical :: stat
            call expect_same_shape([real(real32) :: 1, 2, 3], &
                                   [real(real32) :: 4, 5, 6], "", stat)
            call check(stat .eqv. success, test_name//"real32 rank 1 success")

            call expect_same_shape([real(real32) :: 1, 2, 3], &
                                   [real(real32) :: 4, 5], correct_result, stat)
            call check(stat .eqv. failure, test_name//"real32 rank 1 failure")

            call expect_same_shape(reshape([real(real32) :: 1, 2, 3, 4, 5, 6], [3, 2]), &
                                   reshape([real(real32) :: 4, 5, 6, 7, 8, 9], [3, 2]), "", stat)
            call check(stat, test_name//"real32 rank 2 success")

            call expect_same_shape(reshape([real(real32) :: 1, 2, 3, 4, 5, 6], [3, 2]), &
                                   reshape([real(real32) :: 1, 2, 3, 4, 5, 6], [2, 3]), correct_result, stat)
            call check(.not. stat, test_name//"real32 rank 2 failure")

            call expect_same_shape(reshape([real(real32) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), &
                                   reshape([real(real32) :: 10, 20, 30, 40, 50, 60, 70, 80, 90], [3, 3]), "", stat)
            call check(stat, test_name//"real32 rank 3 success")

            call expect_same_shape(reshape([real(real32) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), &
                                   reshape([real(real32) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 9]), correct_result, stat)
            call check(.not. stat, test_name//"real32 rank 3 failure")
        end block

        block
            logical :: stat
            call expect_same_shape([real(real64) :: 1, 2, 3], &
                                   [real(real64) :: 4, 5, 6], "", stat)
            call check(stat .eqv. success, test_name//"real64 rank 1 success")

            call expect_same_shape([real(real64) :: 1, 2, 3], &
                                   [real(real64) :: 4, 5], correct_result, stat)
            call check(stat .eqv. failure, test_name//"real64 rank 1 failure")

            call expect_same_shape(reshape([real(real64) :: 1, 2, 3, 4, 5, 6], [3, 2]), &
                                   reshape([real(real64) :: 4, 5, 6, 7, 8, 9], [3, 2]), "", stat)
            call check(stat, test_name//"real64 rank 2 success")

            call expect_same_shape(reshape([real(real64) :: 1, 2, 3, 4, 5, 6], [3, 2]), &
                                   reshape([real(real64) :: 1, 2, 3, 4, 5, 6], [2, 3]), correct_result, stat)
            call check(.not. stat, test_name//"real64 rank 2 failure")

            call expect_same_shape(reshape([real(real64) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), &
                                   reshape([real(real64) :: 10, 20, 30, 40, 50, 60, 70, 80, 90], [3, 3]), "", stat)
            call check(stat, test_name//"real64 rank 3 success")

            call expect_same_shape(reshape([real(real64) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), &
                                   reshape([real(real64) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 9]), correct_result, stat)
            call check(.not. stat, test_name//"real64 rank 3 failure")
        end block
    end subroutine test_sameShape_expect

    !| subroutine expect_true/falseのテスト
    subroutine test_logical_expect()
        use :: assertion_logical_expect
        use :: assertion_logical_assert
        implicit none
        character(*), parameter :: test_name = "assert logical expect"

        block
            logical :: stat

            call expect_true(.true., "", stat)
            call check(stat, test_name//"true success")

            call expect_true(.false., correct_result, stat)
            call check(.not. stat, test_name//"true failure")
        end block

        block
            logical :: stat

            call expect_false(.false., "", stat)
            call check(stat, test_name//"false success")

            call expect_false(.true., correct_result, stat)
            call check(.not. stat, test_name//"false failure")
        end block
    end subroutine test_logical_expect

    !| function compareArrayValueのテスト
    subroutine test_equal_compareArrayValue()
        use :: assert_equal_compareArrayValues, only:are_values_of_all_elements_equal
        implicit none
        character(*), parameter :: test_name = "assert equal compare array value"

        block
            call check(are_values_of_all_elements_equal([integer(int32) :: 1, 2, 3], &
                                                        [integer(int32) :: 1, 2, 3]), test_name//"int32 rank 1 success")

            call check(.not. are_values_of_all_elements_equal([integer(int32) :: 1, 2, 3], &
                                                              [integer(int32) :: 4, 5, 6]), test_name//"int32 rank 1 failure")

            call check(are_values_of_all_elements_equal(reshape([integer(int32) :: 1, 2, 3, 4, 5, 6], [3, 2]), &
                                                        reshape([integer(int32) :: 1, 2, 3, 4, 5, 6], [3, 2])), &
                       test_name//"int32 rank 2 success")
            call check(.not. are_values_of_all_elements_equal(reshape([integer(int32) :: 1, 2, 3, 4, 5, 6], [3, 2]), &
                                                              reshape([integer(int32) :: 4, 5, 6, 7, 8, 9], [3, 2])), &
                       test_name//"int32 rank 2 failure")

            call check(are_values_of_all_elements_equal(reshape([integer(int32) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), &
                                                        reshape([integer(int32) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])), &
                       test_name//"int32 rank 3 success")
            call check(.not. are_values_of_all_elements_equal(reshape([integer(int32) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), &
                                                              reshape([integer(int32) :: 10, 20, 30, 40, 50, 60, 70, 80, 90], [3, 3])), &
                       test_name//"int32 rank 3 failure")
        end block

        block
            call check(are_values_of_all_elements_equal([real(real32) :: 1, 2, 3], &
                                                        [real(real32) :: 1, 2, 3]), test_name//"real32 rank 1 success")
            call check(.not. are_values_of_all_elements_equal([real(real32) :: 1, 2, 3], &
                                                              [real(real32) :: 4, 5, 6]), test_name//"real32 rank 1 failure")

            call check(are_values_of_all_elements_equal(reshape([real(real32) :: 1, 2, 3, 4, 5, 6], [3, 2]), &
                                                        reshape([real(real32) :: 1, 2, 3, 4, 5, 6], [3, 2])), &
                       test_name//"real32 rank 2 success")
            call check(.not. are_values_of_all_elements_equal(reshape([real(real32) :: 1, 2, 3, 4, 5, 6], [3, 2]), &
                                                              reshape([real(real32) :: 4, 5, 6, 7, 8, 9], [3, 2])), &
                       test_name//"real32 rank 2 failure")

            call check(are_values_of_all_elements_equal(reshape([real(real32) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), &
                                                        reshape([real(real32) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])), &
                       test_name//"real32 rank 3 success")
            call check(.not. are_values_of_all_elements_equal(reshape([real(real32) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), &
                                                              reshape([real(real32) :: 10, 20, 30, 40, 50, 60, 70, 80, 90], [3, 3])), &
                       test_name//"real32 rank 3 failure")
        end block

        block
            call check(are_values_of_all_elements_equal([real(real64) :: 1, 2, 3], &
                                                        [real(real64) :: 1, 2, 3]), test_name//"real64 rank 1 success")
            call check(.not. are_values_of_all_elements_equal([real(real64) :: 1, 2, 3], &
                                                              [real(real64) :: 4, 5, 6]), test_name//"real64 rank 1 failure")

            call check(are_values_of_all_elements_equal(reshape([real(real64) :: 1, 2, 3, 4, 5, 6], [3, 2]), &
                                                        reshape([real(real64) :: 1, 2, 3, 4, 5, 6], [3, 2])), &
                       test_name//"real64 rank 2 success")
            call check(.not. are_values_of_all_elements_equal(reshape([real(real64) :: 1, 2, 3, 4, 5, 6], [3, 2]), &
                                                              reshape([real(real64) :: 4, 5, 6, 7, 8, 9], [3, 2])), &
                       test_name//"real64 rank 2 failure")

            call check(are_values_of_all_elements_equal(reshape([real(real64) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), &
                                                        reshape([real(real64) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])), &
                       test_name//"real64 rank 3 success")
            call check(.not. are_values_of_all_elements_equal(reshape([real(real64) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), &
                                                              reshape([real(real64) :: 10, 20, 30, 40, 50, 60, 70, 80, 90], [3, 3])), &
                       test_name//"real64 rank 3 failure")
        end block

        block
            call check(are_values_of_all_elements_equal(["a", "r", "r", "a", "y"], &
                                                        ["a", "r", "r", "a", "y"]), test_name//"char array rank 1 success")
            call check(.not. are_values_of_all_elements_equal(["a", "r", "r", "a", "y"], &
                                                              ["b", "s", "s", "b", "z"]), test_name//"char array rank 1 failure")
        end block
    end subroutine test_equal_compareArrayValue

    !| subroutine expect_equalのテスト
    subroutine test_equal_expect()
        use :: assertion_equal_expect
        use :: assertion_equal_assert
        implicit none
        character(*), parameter :: test_name = "assert equal expect_equal "

        block

            logical :: stat

            !------------------------------------------------------------------!
            call expect_equal(1_int8, 1_int8, "", stat, verbose=.false.)
            call check(stat .eqv. success, test_name//"int8 success")

            call expect_equal(1_int8, 1_int8, "", stat, verbose=.true.)
            call check(stat .eqv. success, test_name//"int8 success")

            call expect_equal(1_int8, 2_int8, correct_result, stat, verbose=.false.)
            call check(stat .eqv. failure, test_name//"int8 failure")

            call expect_equal(1_int8, 2_int8, correct_result, stat, verbose=.true.)
            call check(stat .eqv. failure, test_name//"int8 failure")

            !------------------------------------------------------------------!
            call expect_equal(1_int16, 1_int16, "", stat, verbose=.false.)
            call check(stat .eqv. success, test_name//"int16 success")

            call expect_equal(1_int16, 1_int16, "", stat, verbose=.true.)
            call check(stat .eqv. success, test_name//"int16 success")

            call expect_equal(1_int16, 2_int16, correct_result, stat, verbose=.false.)
            call check(stat .eqv. failure, test_name//"int16 failure")

            call expect_equal(1_int16, 2_int16, correct_result, stat, verbose=.true.)
            call check(stat .eqv. failure, test_name//"int16 failure")

            !------------------------------------------------------------------!
            call expect_equal(1_int32, 1_int32, "", stat, verbose=.false.)
            call check(stat .eqv. success, test_name//"int32 success")

            call expect_equal(1_int32, 1_int32, "", stat, verbose=.true.)
            call check(stat .eqv. success, test_name//"int32 success")

            call expect_equal(1_int32, 2_int32, correct_result, stat, verbose=.false.)
            call check(stat .eqv. failure, test_name//"int32 failure")

            call expect_equal(1_int32, 2_int32, correct_result, stat, verbose=.true.)
            call check(stat .eqv. failure, test_name//"int32 failure")

            !------------------------------------------------------------------!
            call expect_equal(1_int64, 1_int64, "", stat, verbose=.false.)
            call check(stat .eqv. success, test_name//"int64 success")

            call expect_equal(1_int64, 1_int64, "", stat, verbose=.true.)
            call check(stat .eqv. success, test_name//"int64 success")

            call expect_equal(1_int64, 2_int64, correct_result, stat, verbose=.false.)
            call check(stat .eqv. failure, test_name//"int64 failure")

            call expect_equal(1_int64, 2_int64, correct_result, stat, verbose=.true.)
            call check(stat .eqv. failure, test_name//"int64 failure")
        end block

        block
            logical :: stat

            !------------------------------------------------------------------!
            call expect_equal(1.0_real32, 1.0_real32, "", stat, verbose=.false.)
            call check(stat .eqv. success, test_name//"real32 success")

            call expect_equal(1.0_real32, 1.0_real32, "", stat, verbose=.true.)
            call check(stat .eqv. success, test_name//"real32 success")

            call expect_equal(1.0_real32, 2.0_real32, correct_result, stat, verbose=.false.)
            call check(stat .eqv. failure, test_name//"real32 failure")

            call expect_equal(1.0_real32, 2.0_real32, correct_result, stat, verbose=.true.)
            call check(stat .eqv. failure, test_name//"real32 failure")

            call expect_equal(0.0_real32, 2.0_real32, "", stat, tolerance=2.0_real32, verbose=.true.)
            call check(stat .eqv. success, test_name//"real32 tolenace success")

            !------------------------------------------------------------------!
            call expect_equal(1.0_real64, 1.0_real64, "", stat, verbose=.false.)
            call check(stat .eqv. success, test_name//"real64 success")

            call expect_equal(1.0_real64, 1.0_real64, "", stat, verbose=.true.)
            call check(stat .eqv. success, test_name//"real64 success")

            call expect_equal(1.0_real64, 2.0_real64, correct_result, stat, verbose=.false.)
            call check(stat .eqv. failure, test_name//"real64 failure")

            call expect_equal(1.0_real64, 2.0_real64, correct_result, stat, verbose=.true.)
            call check(stat .eqv. failure, test_name//"real64 failure")

            call expect_equal(0.0_real64, 2.0_real64, "", stat, tolerance=2.0_real64, verbose=.true.)
            call check(stat .eqv. success, test_name//"real64 tolenace success")
        end block

        block
            logical :: stat
            call expect_equal([integer(int32) :: 1, 2, 3], &
                              [integer(int32) :: 1, 2, 3], "", stat)
            call check(stat .eqv. success, test_name//"int32 rank 1 success")

            call expect_equal([integer(int32) :: 1, 2, 3], &
                              [integer(int32) :: 4, 5, 6], correct_result, stat)
            call check(stat .eqv. failure, test_name//"int32 rank 1 failure")

            call expect_equal(reshape([integer(int32) :: 1, 2, 3, 4, 5, 6], [3, 2]), &
                              reshape([integer(int32) :: 1, 2, 3, 4, 5, 6], [3, 2]), "", stat)
            call check(stat, test_name//"int32 rank 2 success")

            call expect_equal(reshape([integer(int32) :: 1, 2, 3, 4, 5, 6], [3, 2]), &
                              reshape([integer(int32) :: 4, 5, 6, 7, 8, 9], [3, 2]), correct_result, stat)
            call check(.not. stat, test_name//"int32 rank 2 failure")

            call expect_equal(reshape([integer(int32) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), &
                              reshape([integer(int32) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), "", stat)
            call check(stat, test_name//"int32 rank 3 success")

            call expect_equal(reshape([integer(int32) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), &
                              reshape([integer(int32) :: 10, 20, 30, 40, 50, 60, 70, 80, 90], [3, 3]), correct_result, stat)
            call check(.not. stat, test_name//"int32 rank 3 failure")
        end block

        block
            logical :: stat
            call expect_equal([real(real32) :: 1, 2, 3], &
                              [real(real32) :: 1, 2, 3], "", stat)
            call check(stat .eqv. success, test_name//"real32 rank 1 success")

            call expect_equal([real(real32) :: 1, 2, 3], &
                              [real(real32) :: 4, 5, 6], correct_result, stat)
            call check(stat .eqv. failure, test_name//"real32 rank 1 failure")

            call expect_equal(reshape([real(real32) :: 1, 2, 3, 4, 5, 6], [3, 2]), &
                              reshape([real(real32) :: 1, 2, 3, 4, 5, 6], [3, 2]), "", stat)
            call check(stat, test_name//"real32 rank 2 success")

            call expect_equal(reshape([real(real32) :: 1, 2, 3, 4, 5, 6], [3, 2]), &
                              reshape([real(real32) :: 4, 5, 6, 7, 8, 9], [3, 2]), correct_result, stat)
            call check(.not. stat, test_name//"real32 rank 2 failure")

            call expect_equal(reshape([real(real32) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), &
                              reshape([real(real32) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), "", stat)
            call check(stat, test_name//"real32 rank 3 success")

            call expect_equal(reshape([real(real32) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), &
                              reshape([real(real32) :: 10, 20, 30, 40, 50, 60, 70, 80, 90], [3, 3]), correct_result, stat)
            call check(.not. stat, test_name//"real32 rank 3 failure")
        end block

        block
            logical :: stat
            call expect_equal([real(real64) :: 1, 2, 3], &
                              [real(real64) :: 1, 2, 3], "", stat)
            call check(stat .eqv. success, test_name//"real64 rank 1 success")

            call expect_equal([real(real64) :: 1, 2, 3], &
                              [real(real64) :: 4, 5, 6], correct_result, stat)
            call check(stat .eqv. failure, test_name//"real64 rank 1 failure")

            call expect_equal(reshape([real(real64) :: 1, 2, 3, 4, 5, 6], [3, 2]), &
                              reshape([real(real64) :: 1, 2, 3, 4, 5, 6], [3, 2]), "", stat)
            call check(stat, test_name//"real64 rank 2 success")

            call expect_equal(reshape([real(real64) :: 1, 2, 3, 4, 5, 6], [3, 2]), &
                              reshape([real(real64) :: 4, 5, 6, 7, 8, 9], [3, 2]), correct_result, stat)
            call check(.not. stat, test_name//"real64 rank 2 failure")

            call expect_equal(reshape([real(real64) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), &
                              reshape([real(real64) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), "", stat)
            call check(stat, test_name//"real64 rank 3 success")

            call expect_equal(reshape([real(real64) :: 1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3]), &
                              reshape([real(real64) :: 10, 20, 30, 40, 50, 60, 70, 80, 90], [3, 3]), correct_result, stat)
            call check(.not. stat, test_name//"real64 rank 3 failure")
        end block

        block
            logical :: stat

            call expect_equal(.true., .true., "", stat)
            call check(stat .eqv. success, test_name//"logical true success")

            call expect_equal(.true., .false., correct_result, stat)
            call check(stat .eqv. failure, test_name//"logical true failure")

            call expect_equal(.false., .false., "", stat)
            call check(stat .eqv. success, test_name//"logical false success")

            call expect_equal(.false., .true., correct_result, stat)
            call check(stat .eqv. failure, test_name//"logical false failure")
        end block

        block
            logical :: stat

            call expect_equal("string", "string", "", stat)
            call check(stat .eqv. success, test_name//"string success")

            call expect_equal("string ", "string", correct_result, stat)
            call check(stat .eqv. failure, test_name//"string diff length failure")

            call expect_equal("STRING", "string", correct_result, stat)
            call check(stat .eqv. failure, test_name//"string diff content failure")
        end block

        block
            logical :: stat

            call expect_equal(["c", "h", "a", "r"], ["c", "h", "a", "r"], "", stat)
            call check(stat .eqv. success, test_name//"char array success")

            call expect_equal(["c", "h", "a", "r", " "], ["c", "h", "a", "r"], "", stat)
            call check(stat .eqv. failure, test_name//"char array diff length failure")

            call expect_equal(["C", "H", "A", "R"], ["c", "h", "a", "r"], correct_result, stat)
            call check(stat .eqv. failure, test_name//"char array diff content failure")
        end block
    end subroutine test_equal_expect

end program test_assertion

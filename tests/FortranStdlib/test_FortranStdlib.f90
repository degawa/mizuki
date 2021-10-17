program test_FortranStdlib
    use, intrinsic :: iso_fortran_env
    use :: stdlib_logger
    use :: assertion
    implicit none

    call test_global_logger_output()

contains
    subroutine test_global_logger_output()
        implicit none
        character(*), parameter :: test_name = "logger"//" "

        block
            integer(int32) :: log_unit
            integer(int32) :: open_stat, close_stat, io_stat
            character(256) :: log_item

            call global_logger%add_log_file("log_test.txt", unit=log_unit, stat=open_stat)
            call assert_equal(open_stat, success, test_name//"log file open")

            call global_logger%configure(level=all_level, time_stamp=.false.)
            call global_logger%log_debug(message="test output", &
                                         module="test_FortranStdlib", &
                                         procedure="test_add_log")
            flush (log_unit)

            call global_logger%remove_log_unit(log_unit, stat=close_stat)
            call assert_equal(close_stat, success, test_name//"log file close")

            open (newunit=log_unit, file="log_test.txt", action="read", status="old", position="rewind", iostat=io_stat)
            call assert_equal(io_stat, 0, test_name//"existing log file open")

            read (log_unit, '(A)') log_item
            call assert_equal(trim(log_item), "test_FortranStdlib % test_add_log: DEBUG: test output", &
                              test_name//"log item")
            close (log_unit)
        end block

    end subroutine test_global_logger_output
end program test_FortranStdlib

program test_FortranStdlib
    use :: stdlib_logger
    use, intrinsic :: iso_fortran_env
    implicit none

    call test_console_log()

contains
    subroutine test_console_log()
        call global_logger%configure(level=all_level, time_stamp=.true.)
        call global_logger%log_debug(message="test output", &
                                     module="test_FortranStdlib", &
                                     procedure="test_add_log")
    end subroutine test_console_log
end program test_FortranStdlib

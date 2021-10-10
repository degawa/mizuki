!| 論理型の値を検査する手続を定義したモジュール．
module expectLogical
    use, intrinsic :: iso_fortran_env
    use :: assert_common_checkTrue
    implicit none
    private
    public :: expect_true
    public :: expect_false

contains
    !| 論理型変数の値が`.true.`かを検査する<br>
    ! `.true.`であれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! `.true.`でなければ，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納する
    subroutine expect_true(l, test_name, stat)
        implicit none
        logical, intent(in) :: l
        character(*), intent(in) :: test_name
        logical, intent(out), optional :: stat

        call check_true(l, test_name, stat)
    end subroutine expect_true

    !| 論理型変数の値が`.false.`かを検査する<br>
    ! `.false.`であれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`success`を格納する．<br>
    ! `.false.`でなければ，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`failure`を格納する
    subroutine expect_false(l, test_name, stat)
        implicit none
        logical, intent(in) :: l
        character(*), intent(in) :: test_name
        logical, intent(out), optional :: stat

        call check_true(.not. l, test_name, stat)
    end subroutine expect_false
end module expectLogical

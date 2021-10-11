!| 論理型の値を検査し，
!失敗したときにプログラムを停止する手続を定義したモジュール．
module assertion_logical_assert
    use, intrinsic :: iso_fortran_env
    use :: assertion_logical_expect
    use :: assertion_common_stopOnFailure
    implicit none
    private
    public :: assert_true
    public :: assert_false

contains
    !| 論理型変数の値が`.true.`かを検査する<br>
    ! `.true.`であれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`.true.`を格納する．<br>
    ! `.true.`でなければ，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`.false.`を格納し，
    ! プログラムを停止する
    subroutine assert_true(l, test_name)
        implicit none
        logical, intent(in) :: l
        character(*), intent(in) :: test_name

        logical :: stat

        call expect_true(l, test_name, stat)
        call stop_on_failure(stat)
    end subroutine assert_true

    !| 論理型変数の値が`.false.`かを検査する<br>
    ! `.false.`であれば，`PASSED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`.true.`を格納する．<br>
    ! `.false.`でなければ，`FAILED テスト名`というメッセージを表示し，
    ! `stat`が渡されていれば`stat`に`.false.`を格納し，
    ! プログラムを停止する
    subroutine assert_false(l, test_name)
        implicit none
        logical, intent(in) :: l
        character(*), intent(in) :: test_name

        logical :: stat

        call expect_false(.not. l, test_name, stat)
        call stop_on_failure(stat)
    end subroutine assert_false
end module assertion_logical_assert

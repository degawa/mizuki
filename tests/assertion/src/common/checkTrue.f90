!| テスト結果の成否を調べる手続を定義したモジュール
!
module assertion_common_checkTrue
    use :: assertion_common_status
    implicit none
    private
    public :: check_true

contains

    !| 条件`condition`の成否を調べる<br>
    ! `stat`が渡されると，結果を`stat`に格納する．
    ! `condition`が正の場合は`success`，偽の場合は`failure`を格納する．
    subroutine check_true(condition, test_name, stat)
        implicit none
        !&<
        logical     , intent(in)            :: condition
            !! テストの成否を表す条件
        character(*), intent(in)            :: test_name
            !! 成否を調べたいテストの名前
        logical     , intent(out), optional :: stat
            !! 成否の状態
        !&>

        if (condition) then
            print '(A)', "PASSED "//test_name
            if (present(stat)) stat = success
        else
            print '(A)', "FAILED "//test_name
            if (present(stat)) stat = failure
        end if
    end subroutine check_true
end module assertion_common_checkTrue

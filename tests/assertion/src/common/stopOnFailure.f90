!| テストの成否に応じてプログラムを停止する手続を定義したモジュール
module assertion_common_stopOnFailure
    implicit none
    private
    public :: stop_on_failure

contains

    !| テストの成否を調べ，
    ! 失敗していれば`error stop`でプログラムを停止する
    subroutine stop_on_failure(stat)
        use :: assertion_common_status
        implicit none
        logical, intent(in) :: stat
            !! 成否の状態

        if (stat .eqv. failure) error stop
    end subroutine stop_on_failure
end module assertion_common_stopOnFailure

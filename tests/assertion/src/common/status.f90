!| テスト結果の成否と論理値の対応を定義したモジュール
!
module assertion_common_status
    implicit none
    private

    logical, public, parameter :: success = .true.
        !! テストが成功した状態を論理値の`.true.`で表す
    logical, public, parameter :: failure = .false.
        !! テストが失敗した状態を論理値の`.false.`で表す
end module assertion_common_status

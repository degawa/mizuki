!| テストのassertionに用いる手続を定義したモジュール．
module assert
    use, intrinsic :: iso_fortran_env
    use :: expectEqual
    use :: expectSameShape
    use :: expectLogical
    implicit none
    private
    public :: expect_equal
    public :: expect_same_shape
    public :: expect_true
    public :: expect_false
    public :: show_sammary

contains

    !|テスト結果を表示する<br>
    ! 全て成功したか，失敗したか（その場合は何個のテストが失敗したか）
    ! を表示する
    subroutine show_sammary(module_name, test_results)
        implicit none
        !&<
        character(*), intent(in) :: module_name
            !! テストを実行しているモジュールの名前
        logical     , intent(in) :: test_results(:)
            !! テスト結果
        !&>

        if (are_all_statuses_True(test_results)) then
            print '(A)', "PASSED "//module_name//" all tests"
        else
            print '(A, I0, A)', "FAILED "//module_name, number_of_logical_elements(test_results, value=.false.), " test(s)"
        end if
    end subroutine show_sammary

    !| 全ての状態が真かを調べる<br>
    ! テスト結果（真/偽）を格納した配列を渡して，
    ! テストが全て成功したかを調べる用途に用いる
    pure logical function are_all_statuses_True(stat)
        implicit none

        logical, intent(in) :: stat(:)
            !! テスト結果

        are_all_statuses_True = all(stat .eqv. .true.)
    end function are_all_statuses_True

    !| 論理値の配列の中に，`value`で指定した値が
    ! 何個あるかを数える
    pure function number_of_logical_elements(array, value) result(num_elem)
        implicit none

        logical, intent(in) :: array(:)
            !! テスト結果を格納した配列
        logical, intent(in) :: value
            !! 第1引数`array`の中に何個あるか数えたい値<br>
            !! `.true.`もしくは`.false.`

        integer(int32) :: num_elem
            !! `array`の中にある`value`の数

        num_elem = count(array .eqv. value)
    end function number_of_logical_elements
end module assert

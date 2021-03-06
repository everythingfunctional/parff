module repeat_test
    implicit none
    private

    public :: test_repeat
contains
    function test_repeat() result(tests)
        use veggies, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(2)

        individual_tests(1) = it( &
                "uses the parser the given number of times", check_repeat)
        individual_tests(2) = it( &
                "fails if it can't parse that many", check_not_enough)
        tests = describe("repeat", individual_tests)
    end function

    function check_repeat() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: parsed_items_t, parser_output_t, new_state, repeat_
        use veggies, only: result_t, assert_equals, fail

        type(result_t) :: result_

        type(parser_output_t) :: results

        results = repeat_(parse_a, 2, new_state(var_str("AA")))
        if (results%ok) then
            select type (parsed => results%parsed)
            type is (parsed_items_t)
                result_ = assert_equals(2, size(parsed%items))
            class default
                result_ = fail("Didn't get list back")
            end select
        else
            result_ = fail(results%message%to_string())
        end if
    end function

    function check_not_enough() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: parser_output_t, new_state, repeat_
        use veggies, only: result_t, assert_equals, assert_not

        type(result_t) :: result_

        type(parser_output_t) :: results

        results = repeat_(parse_a, 3, new_state(var_str("AAB")))

        result_ = assert_not(results%ok)
        if (result_%passed()) then
            result_ = &
                    assert_equals("B", results%message%found) &
                    .and.assert_equals("A", results%message%expected(1))
        end if
    end function

    function parse_a(state_) result(result_)
        use parff, only: parser_output_t, state_t, parse_char

        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = parse_char("A", state_)
    end function
end module

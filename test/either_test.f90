module either_test
    implicit none
    private

    public :: test_either
contains
    function test_either() result(tests)
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "Returns the first result if it passed", check_first_pass)
        individual_tests(2) = it( &
                "Returns the second result if it passed", check_second_pass)
        individual_tests(3) = it( &
                "Returns the error if neither passed", check_both_fail)
        tests = describe("either", individual_tests)
    end function

    pure function check_first_pass() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: parsed_character_t, parser_output_t, either, new_state
        use vegetables, only: result_t, assert_equals, assert_not, assert_that, fail

        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        parse_result = either(parse_f, parse_a, new_state(var_str("First")))

        result_ = &
                assert_that(parse_result%ok, "Got result", "Didn't get result") &
                .and.assert_not(parse_result%empty, "Wasn't empty", "Was empty")
        if (result_%passed()) then
            select type (the_char => parse_result%parsed)
            type is (parsed_character_t)
                result_ = &
                        assert_equals("F", the_char%value_) &
                        .and.assert_equals("irst", parse_result%remaining)
            class default
                result_ = fail("Didn't get a character back")
            end select
        end if
    end function

    pure function check_second_pass() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: parsed_character_t, parser_output_t, either, new_state
        use vegetables, only: result_t, assert_equals, assert_not, assert_that, fail

        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        parse_result = either(parse_a, parse_f, new_state(var_str("First")))

        result_ = &
                assert_that(parse_result%ok, "Got result", "Didn't get result") &
                .and.assert_not(parse_result%empty, "Wasn't empty", "Was empty")
        if (result_%passed()) then
            select type (the_char => parse_result%parsed)
            type is (parsed_character_t)
                result_ = &
                        assert_equals("F", the_char%value_) &
                        .and.assert_equals("irst", parse_result%remaining)
            class default
                result_ = fail("Didn't get a character back")
            end select
        end if
    end function

    pure function check_both_fail() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: parser_output_t, either, new_state
        use vegetables, only: result_t, assert_equals, assert_not

        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        parse_result = either(parse_a, parse_a, new_state(var_str("First")))

        result_ = &
                assert_not(parse_result%ok) &
                .and.assert_equals("F", parse_result%message%found) &
                .and.assert_equals(2, size(parse_result%message%expected))
    end function

    pure function parse_a(state_) result(result_)
        use parff, only: parser_output_t, state_t, parse_char

        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = parse_char("A", state_)
    end function

    pure function parse_f(state_) result(result_)
        use parff, only: parser_output_t, state_t, parse_char

        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = parse_char("F", state_)
    end function
end module

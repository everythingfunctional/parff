module parse_whitespace_test
    implicit none
    private

    public :: test_parse_whitespace
contains
    function test_parse_whitespace() result(tests)
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "Parsing the first character in a string consumes that character", &
                check_parse_first_character)
        individual_tests(2) = it( &
                "Parsing a different character produces an error", &
                check_parse_different_character)
        individual_tests(3) = it( &
                "Parsing an empty string produces an error", &
                check_parse_empty_string)
        tests = describe("parse_whitespace", individual_tests)
    end function

    pure function check_parse_first_character() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: &
                parsed_character_t, parser_output_t, new_state, parse_whitespace
        use vegetables, only: &
                result_t, assert_equals, assert_not, assert_that, fail

        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        parse_result = parse_whitespace(new_state(var_str(" First")))

        result_ = &
                assert_that(parse_result%ok, "Got result", "Didn't get result") &
                .and.assert_not(parse_result%empty, "Wasn't empty", "Was empty")
        if (result_%passed()) then
            select type (the_char => parse_result%parsed)
            type is (parsed_character_t)
                result_ = &
                        assert_equals(" ", the_char%value_) &
                        .and.assert_equals("First", parse_result%remaining)
            class default
                result_ = fail("Didn't get a character back")
            end select
        end if
    end function

    pure function check_parse_different_character() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: parser_output_t, new_state, parse_whitespace
        use vegetables, only: result_t, assert_equals, assert_not

        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        parse_result = parse_whitespace(new_state(var_str("First")))

        result_ = &
                assert_not(parse_result%ok) &
                .and.assert_equals("F", parse_result%message%found) &
                .and.assert_equals("whitespace", parse_result%message%expected(1))
    end function

    pure function check_parse_empty_string() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: parser_output_t, new_state, parse_whitespace
        use vegetables, only: result_t, assert_equals, assert_not

        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        parse_result = parse_whitespace(new_state(var_str("")))

        result_ = &
                assert_not(parse_result%ok) &
                .and.assert_equals("end of input", parse_result%message%found) &
                .and.assert_equals("whitespace", parse_result%message%expected(1))
    end function
end module

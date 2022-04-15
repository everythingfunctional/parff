module parse_string_test
    implicit none
    private

    public :: test_parse_string
contains
    function test_parse_string() result(tests)
        use veggies, only: test_item_t, describe, it

        type(test_item_t) :: tests

        tests = describe( &
                "parse_string", &
                [ it( &
                        "Parsing the first part of a string consumes that string", &
                        check_pass) &
                , it( &
                        "Parsing something else produces an error", &
                        check_fail) &
                , it( &
                        "Can try to parse an empty string without crashing", &
                        check_parse_empty) &
                ])
    end function

    function check_pass() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: &
                parsed_string_t, parser_output_t, new_state, parse_string
        use veggies, only: &
                result_t, assert_equals, assert_not, assert_that, fail

        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        parse_result = parse_string("Hello", new_state(var_str("Hello World")))

        result_ = &
                assert_that(parse_result%ok, "Got result", "Didn't get result") &
                .and.assert_not(parse_result%empty, "Wasn't empty", "Was empty")
        if (result_%passed()) then
            select type (the_string => parse_result%parsed)
            type is (parsed_string_t)
                result_ = &
                        assert_equals("Hello", the_string%value_) &
                        .and.assert_equals(" World", parse_result%remaining)
            class default
                result_ = fail("Didn't get a string back")
            end select
        end if
    end function

    function check_fail() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: &
                parser_output_t, new_state, parse_string
        use veggies, only: result_t, assert_equals, assert_not

        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        parse_result = parse_string("Hello", new_state(var_str("Help")))

        result_ = &
                assert_not(parse_result%ok) &
                .and.assert_equals("Help", parse_result%message%found) &
                .and.assert_equals("Hello", parse_result%message%expected(1)) &
                .and.assert_equals(4, parse_result%message%position%column)
    end function

    function check_parse_empty() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: &
                parser_output_t, new_state, parse_string
        use veggies, only: result_t, assert_equals, assert_not

        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        parse_result = parse_string("Anything", new_state(var_str("")))

        result_ = &
                assert_not(parse_result%ok) &
                .and.assert_equals("<nothing>", parse_result%message%found) &
                .and.assert_equals("Anything", parse_result%message%expected(1)) &
                .and.assert_equals(1, parse_result%message%position%column)
    end function
end module

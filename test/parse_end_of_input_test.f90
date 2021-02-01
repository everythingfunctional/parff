module parse_end_of_input_test
    use iso_varying_string, only: var_str
    use parff, only: parser_output_t, new_state, parse_end_of_input
    use vegetables, only: &
            result_t, test_item_t, assert_not, assert_that, describe, it

    implicit none
    private
    public :: test_parse_end_of_input
contains
    function test_parse_end_of_input() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "parse_end_of_input", &
                [ it("parsing a non-empty string produces an error", check_non_empty) &
                , it("parsing an empty string succeeds", check_empty) &
                ])
    end function

    function check_non_empty() result(result_)
        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        parse_result = parse_end_of_input(new_state(var_str("non-empty")))

        result_ = assert_not(parse_result%ok(), "parse_result%ok()")
    end function

    function check_empty() result(result_)
        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        parse_result = parse_end_of_input(new_state(var_str("")))

        result_ = assert_that(parse_result%ok(), "parse_result%ok()")
    end function
end module

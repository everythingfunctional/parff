module parse_with_test
    implicit none
    private

    public :: test_parse_with
contains
    function test_parse_with() result(tests)
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(2)

        individual_tests(1) = it( &
                "Gets the parsed result back if successful", &
                check_successful)
        individual_tests(2) = it( &
                "Gets a message back if failed", &
                check_failure)
        tests = describe("parse_with", individual_tests)
    end function

    pure function check_successful() result(result_)
        use parff, only: parse_result_t, parsed_character_t, parse_with
        use vegetables, only: result_t, assert_equals, fail

        type(result_t) :: result_

        type(parse_result_t) :: the_result

        the_result = parse_with(the_parser, "A")

        if (the_result%ok) then
            select type (parsed => the_result%parsed)
            type is (parsed_character_t)
                result_ = assert_equals("A", parsed%value_)
            class default
                result_ = fail("Didn't get a character back")
            end select
        else
            result_ = fail(the_result%message)
        end if
    end function

    pure function check_failure() result(result_)
        use parff, only: parse_result_t, parse_with
        use vegetables, only: result_t, assert_not

        type(result_t) :: result_

        type(parse_result_t) :: the_result

        the_result = parse_with(the_parser, "B")

        result_ = assert_not(the_result%ok, the_result%message)
    end function

    pure function the_parser(state) result(result_)
        use parff, only: parser_output_t, state_t, parse_char

        type(state_t), intent(in) :: state
        type(parser_output_t) :: result_

        result_ = parse_char("A", state)
    end function
end module

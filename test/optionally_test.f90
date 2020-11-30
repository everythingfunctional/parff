module optionally_test
    implicit none
    private

    public :: test_optionally
contains
    function test_optionally() result(tests)
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "parses the given parser if it succeeds", check_parse_succeed)
        individual_tests(2) = it( &
                "parses nothing if the parser fails", check_parse_fails)
        individual_tests(3) = it( &
                "parses nothing if the string is empty", check_parse_empty)
        tests = describe("optionally", individual_tests)
    end function

    pure function check_parse_succeed() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: &
                parsed_character_t, parser_output_t, new_state, optionally
        use vegetables, only: result_t, assert_equals, fail

        type(result_t) :: result_

        type(parser_output_t) :: results

        results = optionally(parse_a, new_state(var_str("AB")))
        if (results%ok) then
            select type (parsed => results%parsed)
            type is (parsed_character_t)
                result_ = &
                        assert_equals("A", parsed%value_, "parsed") &
                        .and.assert_equals("B", results%remaining, "remaining")
            class default
                result_ = fail("Didn't get the character back")
            end select
        else
            result_ = fail(results%message%to_string())
        end if
    end function

    pure function check_parse_fails() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: parser_output_t, new_state, optionally
        use vegetables, only: result_t, assert_that, fail

        type(result_t) :: result_

        type(parser_output_t) :: results

        results = optionally(parse_a, new_state(var_str("BB")))
        if (results%ok) then
            result_ = assert_that(results%empty)
        else
            result_ = fail(results%message%to_string())
        end if
    end function

    pure function check_parse_empty() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: parser_output_t, new_state, optionally
        use vegetables, only: result_t, assert_that, fail

        type(result_t) :: result_

        type(parser_output_t) :: results

        results = optionally(parse_a, new_state(var_str("")))
        if (results%ok) then
            result_ = assert_that(results%empty)
        else
            result_ = fail(results%message%to_string())
        end if
    end function

    pure function parse_a(state_) result(result_)
        use parff, only: parser_output_t, state_t, parse_char

        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = parse_char("A", state_)
    end function
end module

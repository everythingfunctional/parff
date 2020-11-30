module sequence_test
    implicit none
    private

    public :: test_sequence
contains
    function test_sequence() result(tests)
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "When both parses pass, the results are combined", &
                check_both_pass)
        individual_tests(2) = it( &
                "When the first parse fails, that error comes back", &
                check_first_fail)
        individual_tests(3) = it( &
                "When the second parse fails, that error comes back", &
                check_second_fail)
        tests = describe("sequence", individual_tests)
    end function

    pure function check_both_pass() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: parsed_string_t, parser_output_t, new_state, sequence
        use vegetables, only: result_t, assert_equals, assert_that, fail

        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        parse_result = sequence(parse_a, then_parse_b, new_state(var_str("AB")))

        result_ = assert_that(parse_result%ok)
        if (result_%passed()) then
            select type (string => parse_result%parsed)
            type is (parsed_string_t)
                result_ = assert_equals("AB", string%value_)
            class default
                result_ = fail("Didn't get string back")
            end select
        end if
    end function

    pure function check_first_fail() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: parser_output_t, new_state, sequence
        use vegetables, only: result_t, assert_equals, assert_not

        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        parse_result = sequence(parse_a, then_parse_b, new_state(var_str("BB")))

        result_ = assert_not(parse_result%ok)
        if (result_%passed()) then
            result_ = &
                    assert_equals("B", parse_result%message%found) &
                    .and.assert_equals("A", parse_result%message%expected(1))
        end if
    end function

    pure function check_second_fail() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: parser_output_t, new_state, sequence
        use vegetables, only: result_t, assert_equals, assert_not

        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        parse_result = sequence(parse_a, then_parse_b, new_state(var_str("AA")))

        result_ = assert_not(parse_result%ok)
        if (result_%passed()) then
            result_ = &
                    assert_equals("A", parse_result%message%found) &
                    .and.assert_equals("B", parse_result%message%expected(1))
        end if
    end function

    pure function parse_a(state_) result(result_)
        use parff, only: parser_output_t, state_t, parse_char

        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = parse_char("A", state_)
    end function

    pure function then_parse_b(previous, state_) result(result_)
        use iso_varying_string, only: assignment(=)
        use parff, only: &
                parsed_character_t, &
                parsed_string_t, &
                parsed_value_t, &
                parser_output_t, &
                state_t, &
                parse_char

        class(parsed_value_t), intent(in) :: previous
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        type(parsed_string_t) :: parsed

        result_ = parse_char("B", state_)

        if (result_%ok) then
            select type (previous)
            type is (parsed_character_t)
                select type (next => result_%parsed)
                type is (parsed_character_t)
                    parsed%value_ = previous%value_ // next%value_
                end select
            end select
            deallocate(result_%parsed)
            allocate(result_%parsed, source = parsed)
        end if
    end function
end module

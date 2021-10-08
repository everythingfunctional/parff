module sequence_test
    use iso_varying_string, only: var_str
    use parff, only: &
            message_t, &
            parsed_character_t, &
            parsed_string_t, &
            parsed_value_t, &
            parser_output_t, &
            state_t, &
            parse_char, &
            new_state, &
            sequence
    use vegetables, only: &
            result_t, &
            test_item_t, &
            assert_equals, &
            assert_not, &
            assert_that, &
            describe, &
            fail, &
            it

    implicit none
    private

    public :: test_sequence
contains
    function test_sequence() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "sequence", &
                [ it( &
                        "When both parses pass, the results are combined", &
                        check_both_pass) &
                , it( &
                        "When the first parse fails, that error comes back", &
                        check_first_fail) &
                , it( &
                        "When the second parse fails, that error comes back", &
                        check_second_fail) &
                ])
    end function

    function check_both_pass() result(result_)
        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        parse_result = sequence(parse_a, then_parse_b, new_state(var_str("AB")))

        result_ = assert_that(parse_result%ok)
        if (result_%passed()) then
            select type (string => parse_result%parsed)
            type is (parsed_string_t)
                result_ = assert_equals("AB", string%value_())
            class default
                result_ = fail("Didn't get string back")
            end select
        end if
    end function

    function check_first_fail() result(result_)
        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        parse_result = sequence(parse_a, then_parse_b, new_state(var_str("BB")))

        result_ = &
                assert_not(parse_result%ok, "parse_result%ok") &
                .and.assert_that(parse_result%empty, "parse_result%empty")
        if (result_%passed()) then
            result_ = &
                    assert_equals("B", parse_result%message%found) &
                    .and.assert_equals("A", parse_result%message%expected(1))
        end if
    end function

    function check_second_fail() result(result_)
        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        parse_result = sequence(parse_a, then_parse_b, new_state(var_str("AA")))

        result_ = &
                assert_not(parse_result%ok, "parse_result%ok") &
                .and.assert_that(parse_result%empty, "parse_result%empty")
        if (result_%passed()) then
            result_ = &
                    assert_equals("A", parse_result%message%found) &
                    .and.assert_equals("B", parse_result%message%expected(1))
        end if
    end function

    function parse_a(state_) result(result_)
        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = parse_char("A", state_)
    end function

    function then_parse_b(previous, state_) result(result_)
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
                    parsed = parsed_string_t(previous%value_() // next%value_())
                end select
            end select
            result_ = result_%with_parsed_value(parsed)
        end if
    end function
end module

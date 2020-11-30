module many_with_separator_test
    implicit none
    private

    public :: test_many_with_separator
contains
    function test_many_with_separator() result(tests)
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(5)

        individual_tests(1) = it("can parse one item", check_one)
        individual_tests(2) = it( &
                "can parse one item followed by a separator", &
                check_one_with_separator)
        individual_tests(3) = it( &
                "parses until the parser doesn't match", check_many)
        individual_tests(4) = it( &
                "leaves a trailing separator", check_many_with_separator)
        individual_tests(5) = it( &
                "returns empty if the first result doesn't match", check_none)
        tests = describe("many_with_separator", individual_tests)
    end function

    pure function check_one() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: &
                parsed_items_t, parser_output_t, many_with_separator, new_state
        use vegetables, only: result_t, assert_equals, fail

        type(result_t) :: result_

        type(parser_output_t) :: results

        results = many_with_separator(parse_a, parse_comma, new_state(var_str("AB")))
        if (results%ok) then
            select type (parsed => results%parsed)
            type is (parsed_items_t)
                result_ = &
                        assert_equals(1, size(parsed%items)) &
                        .and.assert_equals("B", results%remaining)
            class default
                result_ = fail("Didn't get list back")
            end select
        else
            result_ = fail(results%message%to_string())
        end if
    end function

    pure function check_one_with_separator() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: &
                parsed_character_t, &
                parsed_items_t, &
                parser_output_t, &
                many_with_separator, &
                new_state
        use vegetables, only: result_t, assert_equals, fail

        type(result_t) :: result_

        type(parser_output_t) :: results

        results = many_with_separator(parse_a, parse_comma, new_state(var_str("A,B")))
        if (results%ok) then
            select type (parsed => results%parsed)
            type is (parsed_items_t)
                result_ = assert_equals(1, size(parsed%items))
                if (result_%passed()) then
                    select type (the_item => parsed%items(1)%item)
                    type is (parsed_character_t)
                        result_ = &
                                assert_equals("A", the_item%value_) &
                                .and.assert_equals(",B", results%remaining)
                    end select
                end if
            class default
                result_ = fail("Didn't get list back")
            end select
        else
            result_ = fail(results%message%to_string())
        end if
    end function

    pure function check_many() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: &
                parsed_items_t, parser_output_t, many_with_separator, new_state
        use vegetables, only: result_t, assert_equals, fail

        type(result_t) :: result_

        type(parser_output_t) :: results

        results = many_with_separator(parse_a, parse_comma, new_state(var_str("A,A,AB")))
        if (results%ok) then
            select type (parsed => results%parsed)
            type is (parsed_items_t)
                result_ = &
                        assert_equals(3, size(parsed%items))&
                        .and.assert_equals("B", results%remaining)
            class default
                result_ = fail("Didn't get list back")
            end select
        else
            result_ = fail(results%message%to_string())
        end if
    end function

    pure function check_many_with_separator() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: &
                parsed_items_t, parser_output_t, many_with_separator, new_state
        use vegetables, only: result_t, assert_equals, fail

        type(result_t) :: result_

        type(parser_output_t) :: results

        results = many_with_separator(parse_a, parse_comma, new_state(var_str("A,A,A,B")))
        if (results%ok) then
            select type (parsed => results%parsed)
            type is (parsed_items_t)
                result_ = &
                        assert_equals(3, size(parsed%items))&
                        .and.assert_equals(",B", results%remaining)
            class default
                result_ = fail("Didn't get list back")
            end select
        else
            result_ = fail(results%message%to_string())
        end if
    end function

    pure function check_none() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: parser_output_t, many_with_separator, new_state
        use vegetables, only: result_t, assert_that, fail

        type(result_t) :: result_

        type(parser_output_t) :: results

        results = many_with_separator(parse_a, parse_comma, new_state(var_str("B,A,A")))
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

    pure function parse_comma(state_) result(result_)
        use parff, only: parser_output_t, state_t, parse_char

        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = parse_char(",", state_)
    end function
end module

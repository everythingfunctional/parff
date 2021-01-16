module many1_with_separator_test
    implicit none
    private

    public :: test_many1_with_separator
contains
    function test_many1_with_separator() result(tests)
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
                "fails if the first result doesn't match", check_none)
        tests = describe("many1_with_separator", individual_tests)
    end function

    function check_one() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: &
                parsed_items_t, parser_output_t, many1_with_separator, new_state
        use vegetables, only: result_t, assert_equals, fail

        type(result_t) :: result_

        type(parser_output_t) :: results

        results = many1_with_separator(parse_a, parse_comma, new_state(var_str("AB")))
        if (results%ok()) then
            select type (parsed => results%parsed_)
            type is (parsed_items_t)
                result_ = &
                        assert_equals(1, size(parsed%items())) &
                        .and.assert_equals("B", results%remaining_)
            class default
                result_ = fail("Didn't get list back")
            end select
        else
            associate(message => results%message())
                result_ = fail(message%to_string())
            end associate
        end if
    end function

    function check_one_with_separator() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: &
                parsed_items_t, parser_output_t, many1_with_separator, new_state
        use vegetables, only: result_t, assert_equals, fail

        type(result_t) :: result_

        type(parser_output_t) :: results

        results = many1_with_separator(parse_a, parse_comma, new_state(var_str("A,B")))
        if (results%ok()) then
            select type (parsed => results%parsed_)
            type is (parsed_items_t)
                result_ = &
                        assert_equals(1, size(parsed%items())) &
                        .and.assert_equals(",B", results%remaining_)
            class default
                result_ = fail("Didn't get list back")
            end select
        else
            associate(message => results%message())
                result_ = fail(message%to_string())
            end associate
        end if
    end function

    function check_many() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: &
                parsed_items_t, parser_output_t, many1_with_separator, new_state
        use vegetables, only: result_t, assert_equals, fail

        type(result_t) :: result_

        type(parser_output_t) :: results

        results = many1_with_separator(parse_a, parse_comma, new_state(var_str("A,A,AB")))
        if (results%ok()) then
            select type (parsed => results%parsed_)
            type is (parsed_items_t)
                result_ = &
                        assert_equals(3, size(parsed%items())) &
                        .and.assert_equals("B", results%remaining_)
            class default
                result_ = fail("Didn't get list back")
            end select
        else
            associate(message => results%message())
                result_ = fail(message%to_string())
            end associate
        end if
    end function

    function check_many_with_separator() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: &
                parsed_items_t, parser_output_t, many1_with_separator, new_state
        use vegetables, only: result_t, assert_equals, fail

        type(result_t) :: result_

        type(parser_output_t) :: results

        results = many1_with_separator(parse_a, parse_comma, new_state(var_str("A,A,A,B")))
        if (results%ok()) then
            select type (parsed => results%parsed_)
            type is (parsed_items_t)
                result_ = &
                        assert_equals(3, size(parsed%items())) &
                        .and.assert_equals(",B", results%remaining_)
            class default
                result_ = fail("Didn't get list back")
            end select
        else
            associate(message => results%message())
                result_ = fail(message%to_string())
            end associate
        end if
    end function

    function check_none() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: message_t, parser_output_t, many1_with_separator, new_state
        use vegetables, only: result_t, assert_not

        type(result_t) :: result_

        type(message_t) :: message
        type(parser_output_t) :: results

        results = many1_with_separator(parse_a, parse_comma, new_state(var_str("BAA")))
        message = results%message()
        result_ = assert_not(results%ok(), message%to_string())
    end function

    function parse_a(state_) result(result_)
        use parff, only: parser_output_t, state_t, parse_char

        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = parse_char("A", state_)
    end function

    function parse_comma(state_) result(result_)
        use parff, only: parser_output_t, state_t, parse_char

        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = parse_char(",", state_)
    end function
end module

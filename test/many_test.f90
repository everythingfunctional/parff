module many_test
    implicit none
    private

    public :: test_many
contains
    function test_many() result(tests)
        use vegetables, only: test_item_t, describe, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(3)

        individual_tests(1) = it("can parse one item", check_one)
        individual_tests(2) = it( &
                "parses until the parser doesn't match", check_many)
        individual_tests(3) = it( &
                "returns empty if the first result doesn't match", check_none)
        tests = describe("many", individual_tests)
    end function

    function check_one() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: parsed_items_t, parser_output_t, many, new_state
        use vegetables, only: result_t, assert_equals, fail

        type(result_t) :: result_

        type(parser_output_t) :: results

        results = many(parse_a, new_state(var_str("AB")))
        if (results%ok) then
            select type (parsed => results%parsed())
            type is (parsed_items_t)
                result_ = &
                        assert_equals(1, size(parsed%items())) &
                        .and.assert_equals("B", results%remaining())
            class default
                result_ = fail("Didn't get list back")
            end select
        else
            result_ = fail(results%message%to_string())
        end if
    end function

    function check_many() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: parsed_items_t, parser_output_t, many, new_state
        use vegetables, only: result_t, assert_equals, fail

        type(result_t) :: result_

        type(parser_output_t) :: results

        results = many(parse_a, new_state(var_str("AAAB")))
        if (results%ok) then
            select type (parsed => results%parsed())
            type is (parsed_items_t)
                result_ = &
                        assert_equals(3, size(parsed%items())) &
                        .and.assert_equals("B", results%remaining())
            class default
                result_ = fail("Didn't get list back")
            end select
        else
            result_ = fail(results%message%to_string())
        end if
    end function

    function check_none() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: parser_output_t, many, new_state
        use vegetables, only: result_t, assert_that, fail

        type(result_t) :: result_

        type(parser_output_t) :: results

        results = many(parse_a, new_state(var_str("BAA")))
        if (results%ok) then
            result_ = assert_that(results%empty)
        else
            result_ = fail(results%message%to_string())
        end if
    end function

    function parse_a(state_) result(result_)
        use parff, only: parser_output_t, state_t, parse_char

        type(state_t), intent(in) :: state_
        type(parser_output_t) :: result_

        result_ = parse_char("A", state_)
    end function
end module

module parse_integer_test
    use iso_varying_string, only: varying_string
    use vegetables, only: Input_t

    implicit none
    private

    type, extends(Input_t) :: number_input_t
        type(varying_string) :: string
        integer :: value_
    end type

    type, extends(Input_t) :: invalid_input_t
        type(varying_string) :: string
    end type

    public :: test_parse_integer
contains
    function test_parse_integer() result(tests)
        use iso_varying_string, only: var_str
        use vegetables, only: Example_t, test_item_t, describe, Example, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(3)
        type(Example_t) :: invalid_examples(3)
        type(Example_t) :: number_examples(7)

        number_examples(1) = Example(number_input_t(var_str("0"), 0))
        number_examples(2) = Example(number_input_t(var_str("-0"), 0))
        number_examples(3) = Example(number_input_t(var_str("+0"), 0))
        number_examples(4) = Example(number_input_t(var_str("1"), 1))
        number_examples(5) = Example(number_input_t(var_str("-1"), -1))
        number_examples(6) = Example(number_input_t(var_str("+1"), 1))
        number_examples(7) = Example(number_input_t(var_str("-321"), -321))

        invalid_examples(1) = Example(invalid_input_t(var_str("a")))
        invalid_examples(2) = Example(invalid_input_t(var_str("-b")))
        invalid_examples(3) = Example(invalid_input_t(var_str("+c")))

        individual_tests(1) = it( &
                "Can parse various integers", &
                number_examples, &
                check_parse_integer)
        individual_tests(2) = it( &
                "Parsing invalid integers produce errors", &
                invalid_examples, &
                check_parse_invalid)
        individual_tests(3) = it( &
                "Parsing an empty string produces an error", check_parse_empty)
        tests = describe("parse_integer", individual_tests)
    end function

    pure function check_parse_integer(input) result(result_)
        use parff, only: &
                parsed_integer_t, parser_output_t, new_state, parse_integer
        use vegetables, only: Input_t, result_t, assert_equals, fail

        class(Input_t), intent(in) :: input
        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        select type (input)
        type is (number_input_t)
            parse_result = parse_integer(new_state(input%string))
            if (parse_result%ok) then
                select type (parsed => parse_result%parsed)
                type is (parsed_integer_t)
                    result_ = assert_equals( &
                            input%value_, parsed%value_, input%string)
                class default
                    result_ = fail("Didn't get an integer back")
                end select
            else
                result_ = fail(parse_result%message%to_string())
            end if
        class default
            result_ = fail("Expected to get a number_input_t")
        end select
    end function

    pure function check_parse_invalid(input) result(result_)
        use parff, only: parser_output_t, new_state, parse_integer
        use vegetables, only: Input_t, result_t, assert_not, fail

        class(Input_t), intent(in) :: input
        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        select type (input)
        type is (invalid_input_t)
            parse_result = parse_integer(new_state(input%string))
            result_ = assert_not( &
                    parse_result%ok, parse_result%message%to_string())
        class default
            result_ = fail("Expected to get an invalid_input_tt")
        end select
    end function

    pure function check_parse_empty() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: parser_output_t, new_state, parse_integer
        use vegetables, only: result_t, assert_not

        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        parse_result = parse_integer(new_state(var_str("")))
        result_ = assert_not( &
                parse_result%ok, parse_result%message%to_string())
    end function
end module

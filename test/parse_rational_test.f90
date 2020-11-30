module parse_rational_test
    use iso_varying_string, only: varying_string
    use vegetables, only: Input_t

    implicit none
    private

    type, extends(Input_t) :: number_input_t
        type(varying_string) :: string
        double precision :: value_
    end type

    type, extends(Input_t) :: invalid_input_t
        type(varying_string) :: string
    end type

    public :: test_parse_rational
contains
    function test_parse_rational() result(tests)
        use iso_varying_string, only: var_str
        use vegetables, only: Example_t, test_item_t, describe, Example, it

        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(3)
        type(Example_t) :: invalid_examples(6)
        type(Example_t) :: number_examples(10)

        number_examples(1) = Example(number_input_t(var_str("1"), 1.0d0))
        number_examples(2) = Example(number_input_t(var_str("-2"), -2.0d0))
        number_examples(3) = Example(number_input_t(var_str("+3"), 3.0d0))
        number_examples(4) = Example(number_input_t(var_str("4."), 4.0d0))
        number_examples(5) = Example(number_input_t(var_str("5.0"), 5.0d0))
        number_examples(6) = Example(number_input_t(var_str(".6"), 0.6d0))
        number_examples(7) = Example(number_input_t(var_str("7e8"), 7.0d8))
        number_examples(8) = Example(number_input_t(var_str("9.E-1"), 9.0d-1))
        number_examples(9) = Example(number_input_t(var_str(".2d+3"), 0.2d3))
        number_examples(10) = Example(number_input_t(var_str("4.0D5"), 4.0d5))

        invalid_examples(1) = Example(invalid_input_t(var_str("a")))
        invalid_examples(2) = Example(invalid_input_t(var_str("-b")))
        invalid_examples(3) = Example(invalid_input_t(var_str("+c")))
        invalid_examples(4) = Example(invalid_input_t(var_str(".")))
        invalid_examples(5) = Example(invalid_input_t(var_str(".e")))
        invalid_examples(6) = Example(invalid_input_t(var_str("-d+")))

        individual_tests(1) = it( &
                "Can parse various numbers", &
                number_examples, &
                check_parse_rational)
        individual_tests(2) = it( &
                "Parsing invalid numbers produce errors", &
                invalid_examples, &
                check_parse_invalid)
        individual_tests(3) = it( &
                "Parsing an empty string produces an error", check_parse_empty)
        tests = describe("parse_rational", individual_tests)
    end function

    pure function check_parse_rational(input) result(result_)
        use parff, only: &
                parsed_rational_t, parser_output_t, new_state, parse_rational
        use vegetables, only: Input_t, result_t, assert_equals, fail

        class(Input_t), intent(in) :: input
        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        select type (input)
        type is (number_input_t)
            parse_result = parse_rational(new_state(input%string))
            if (parse_result%ok) then
                select type (parsed => parse_result%parsed)
                type is (parsed_rational_t)
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
        use parff, only: parser_output_t, new_state, parse_rational
        use vegetables, only: Input_t, result_t, assert_not, fail

        class(Input_t), intent(in) :: input
        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        select type (input)
        type is (invalid_input_t)
            parse_result = parse_rational(new_state(input%string))
            result_ = assert_not( &
                    parse_result%ok, parse_result%message%to_string())
        class default
            result_ = fail("Expected to get an invalid_input_tt")
        end select
    end function

    pure function check_parse_empty() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: parser_output_t, new_state, parse_rational
        use vegetables, only: Input_t, result_t, assert_not

        type(result_t) :: result_

        type(parser_output_t) :: parse_result

        parse_result = parse_rational(new_state(var_str("")))
        result_ = assert_not( &
                parse_result%ok, parse_result%message%to_string())
    end function
end module

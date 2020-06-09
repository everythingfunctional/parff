module parse_integer_test
    use iso_varying_string, only: VARYING_STRING, var_str
    use parff, only: ParsedInteger_t, ParserOutput_t, newState, parseInteger
    use Vegetables_m, only: &
            Example_t, &
            Input_t, &
            Result_t, &
            TestItem_t, &
            assertEquals, &
            assertNot, &
            describe, &
            Example, &
            fail, &
            it

    implicit none
    private

    type, extends(Input_t) :: NumberInput_t
        type(VARYING_STRING) :: string
        integer :: value_
    end type NumberInput_t

    type, extends(Input_t) :: InvalidInput_t
        type(VARYING_STRING) :: string
    end type InvalidInput_t

    public :: test_parse_integer
contains
    function test_parse_integer() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)
        type(Example_t) :: invalid_examples(3)
        type(Example_t) :: number_examples(7)

        number_examples(1) = Example(NumberInput_t(var_str("0"), 0))
        number_examples(2) = Example(NumberInput_t(var_str("-0"), 0))
        number_examples(3) = Example(NumberInput_t(var_str("+0"), 0))
        number_examples(4) = Example(NumberInput_t(var_str("1"), 1))
        number_examples(5) = Example(NumberInput_t(var_str("-1"), -1))
        number_examples(6) = Example(NumberInput_t(var_str("+1"), 1))
        number_examples(7) = Example(NumberInput_t(var_str("-321"), -321))

        invalid_examples(1) = Example(InvalidInput_t(var_str("a")))
        invalid_examples(2) = Example(InvalidInput_t(var_str("-b")))
        invalid_examples(3) = Example(InvalidInput_t(var_str("+c")))

        individual_tests(1) = it( &
                "Can parse various integers", &
                number_examples, &
                checkParseInteger)
        individual_tests(2) = it( &
                "Parsing invalid integers produce errors", &
                invalid_examples, &
                checkParseInvalid)
        individual_tests(3) = it( &
                "Parsing an empty string produces an error", checkParseEmpty)
        tests = describe("parseInteger", individual_tests)
    end function test_parse_integer

    pure function checkParseInteger(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(ParserOutput_t) :: parse_result

        select type (input)
        type is (NumberInput_t)
            parse_result = parseInteger(newState(input%string))
            if (parse_result%ok) then
                select type (parsed => parse_result%parsed)
                type is (ParsedInteger_t)
                    result_ = assertEquals( &
                            input%value_, parsed%value_, input%string)
                class default
                    result_ = fail("Didn't get an integer back")
                end select
            else
                result_ = fail(parse_result%message%toString())
            end if
        class default
            result_ = fail("Expected to get a NumberInput_t")
        end select
    end function checkParseInteger

    pure function checkParseInvalid(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(ParserOutput_t) :: parse_result

        select type (input)
        type is (InvalidInput_t)
            parse_result = parseInteger(newState(input%string))
            result_ = assertNot( &
                    parse_result%ok, parse_result%message%toString())
        class default
            result_ = fail("Expected to get an InvalidInput_tt")
        end select
    end function checkParseInvalid

    pure function checkParseEmpty() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: parse_result

        parse_result = parseInteger(newState(var_str("")))
        result_ = assertNot( &
                parse_result%ok, parse_result%message%toString())
    end function checkParseEmpty
end module parse_integer_test

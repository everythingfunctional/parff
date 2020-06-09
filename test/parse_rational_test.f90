module parse_rational_test
    use iso_varying_string, only: VARYING_STRING, var_str
    use parff, only: ParsedRational_t, ParserOutput_t, newState, parseRational
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
        double precision :: value_
    end type NumberInput_t

    type, extends(Input_t) :: InvalidInput_t
        type(VARYING_STRING) :: string
    end type InvalidInput_t

    public :: test_parse_rational
contains
    function test_parse_rational() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)
        type(Example_t) :: invalid_examples(6)
        type(Example_t) :: number_examples(10)

        number_examples(1) = Example(NumberInput_t(var_str("1"), 1.0d0))
        number_examples(2) = Example(NumberInput_t(var_str("-2"), -2.0d0))
        number_examples(3) = Example(NumberInput_t(var_str("+3"), 3.0d0))
        number_examples(4) = Example(NumberInput_t(var_str("4."), 4.0d0))
        number_examples(5) = Example(NumberInput_t(var_str("5.0"), 5.0d0))
        number_examples(6) = Example(NumberInput_t(var_str(".6"), 0.6d0))
        number_examples(7) = Example(NumberInput_t(var_str("7e8"), 7.0d8))
        number_examples(8) = Example(NumberInput_t(var_str("9.E-1"), 9.0d-1))
        number_examples(9) = Example(NumberInput_t(var_str(".2d+3"), 0.2d3))
        number_examples(10) = Example(NumberInput_t(var_str("4.0D5"), 4.0d5))

        invalid_examples(1) = Example(InvalidInput_t(var_str("a")))
        invalid_examples(2) = Example(InvalidInput_t(var_str("-b")))
        invalid_examples(3) = Example(InvalidInput_t(var_str("+c")))
        invalid_examples(4) = Example(InvalidInput_t(var_str(".")))
        invalid_examples(5) = Example(InvalidInput_t(var_str(".e")))
        invalid_examples(6) = Example(InvalidInput_t(var_str("-d+")))

        individual_tests(1) = it( &
                "Can parse various numbers", &
                number_examples, &
                checkParseRational)
        individual_tests(2) = it( &
                "Parsing invalid numbers produce errors", &
                invalid_examples, &
                checkParseInvalid)
        individual_tests(3) = it( &
                "Parsing an empty string produces an error", checkParseEmpty)
        tests = describe("parseRational", individual_tests)
    end function test_parse_rational

    pure function checkParseRational(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(ParserOutput_t) :: parse_result

        select type (input)
        type is (NumberInput_t)
            parse_result = parseRational(newState(input%string))
            if (parse_result%ok) then
                select type (parsed => parse_result%parsed)
                type is (ParsedRational_t)
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
    end function checkParseRational

    pure function checkParseInvalid(input) result(result_)
        class(Input_t), intent(in) :: input
        type(Result_t) :: result_

        type(ParserOutput_t) :: parse_result

        select type (input)
        type is (InvalidInput_t)
            parse_result = parseRational(newState(input%string))
            result_ = assertNot( &
                    parse_result%ok, parse_result%message%toString())
        class default
            result_ = fail("Expected to get an InvalidInput_tt")
        end select
    end function checkParseInvalid

    pure function checkParseEmpty() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: parse_result

        parse_result = parseRational(newState(var_str("")))
        result_ = assertNot( &
                parse_result%ok, parse_result%message%toString())
    end function checkParseEmpty
end module parse_rational_test

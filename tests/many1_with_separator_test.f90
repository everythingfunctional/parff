module many1_with_separator_test
    use iso_varying_string, only: var_str
    use parff, only: &
            ParsedItems_t, &
            ParserOutput_t, &
            State_t, &
            many1WithSeparator, &
            newState, &
            parseChar
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertEquals, assertNot, describe, fail, it

    implicit none
    private

    public :: test_many1_with_separator
contains
    function test_many1_with_separator() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(5)

        individual_tests(1) = it("can parse one item", checkOne)
        individual_tests(2) = it( &
                "can parse one item followed by a separator", &
                checkOneWithSeparator)
        individual_tests(3) = it( &
                "parses until the parser doesn't match", checkMany)
        individual_tests(4) = it( &
                "leaves a trailing separator", checkManyWithSeparator)
        individual_tests(5) = it( &
                "fails if the first result doesn't match", checkNone)
        tests = describe("many1WithSeparator", individual_tests)
    end function test_many1_with_separator

    pure function checkOne() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: results

        results = many1WithSeparator(parseA, parseComma, newState(var_str("AB")))
        if (results%ok) then
            select type (parsed => results%parsed)
            type is (ParsedItems_t)
                result_ = &
                        assertEquals(1, size(parsed%items)) &
                        .and.assertEquals("B", results%remaining)
            class default
                result_ = fail("Didn't get list back")
            end select
        else
            result_ = fail(results%message%toString())
        end if
    end function checkOne

    pure function checkOneWithSeparator() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: results

        results = many1WithSeparator(parseA, parseComma, newState(var_str("A,B")))
        if (results%ok) then
            select type (parsed => results%parsed)
            type is (ParsedItems_t)
                result_ = &
                        assertEquals(1, size(parsed%items)) &
                        .and.assertEquals(",B", results%remaining)
            class default
                result_ = fail("Didn't get list back")
            end select
        else
            result_ = fail(results%message%toString())
        end if
    end function checkOneWithSeparator

    pure function checkMany() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: results

        results = many1WithSeparator(parseA, parseComma, newState(var_str("A,A,AB")))
        if (results%ok) then
            select type (parsed => results%parsed)
            type is (ParsedItems_t)
                result_ = &
                        assertEquals(3, size(parsed%items)) &
                        .and.assertEquals("B", results%remaining)
            class default
                result_ = fail("Didn't get list back")
            end select
        else
            result_ = fail(results%message%toString())
        end if
    end function checkMany

    pure function checkManyWithSeparator() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: results

        results = many1WithSeparator(parseA, parseComma, newState(var_str("A,A,A,B")))
        if (results%ok) then
            select type (parsed => results%parsed)
            type is (ParsedItems_t)
                result_ = &
                        assertEquals(3, size(parsed%items)) &
                        .and.assertEquals(",B", results%remaining)
            class default
                result_ = fail("Didn't get list back")
            end select
        else
            result_ = fail(results%message%toString())
        end if
    end function checkManyWithSeparator

    pure function checkNone() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: results

        results = many1WithSeparator(parseA, parseComma, newState(var_str("BAA")))
        result_ = assertNot(results%ok, results%message%toString())
    end function checkNone

    pure function parseA(state_) result(result_)
        type(State_t), intent(in) :: state_
        type(ParserOutput_t) :: result_

        result_ = parseChar("A", state_)
    end function parseA

    pure function parseComma(state_) result(result_)
        type(State_t), intent(in) :: state_
        type(ParserOutput_t) :: result_

        result_ = parseChar(",", state_)
    end function parseComma
end module many1_with_separator_test

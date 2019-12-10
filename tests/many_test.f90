module many_test
    use iso_varying_string, only: var_str
    use parff, only: &
            ParsedItems_t, &
            ParserOutput_t, &
            State_t, &
            many, &
            newState, &
            parseChar
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertEquals, assertThat, describe, fail, it

    implicit none
    private

    public :: test_many
contains
    function test_many() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it("can parse one item", checkOne)
        individual_tests(2) = it( &
                "parses until the parser doesn't match", checkMany)
        individual_tests(3) = it( &
                "returns empty if the first result doesn't match", checkNone)
        tests = describe("many", individual_tests)
    end function test_many

    pure function checkOne() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: results

        results = many(parseA, newState(var_str("AB")))
        if (results%ok) then
            select type (parsed => results%parsed)
            type is (ParsedItems_t)
                result_ = assertEquals(1, size(parsed%items))
            class default
                result_ = fail("Didn't get list back")
            end select
        else
            result_ = fail(results%message%toString())
        end if
    end function checkOne

    pure function checkMany() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: results

        results = many(parseA, newState(var_str("AAAB")))
        if (results%ok) then
            select type (parsed => results%parsed)
            type is (ParsedItems_t)
                result_ = assertEquals(3, size(parsed%items))
            class default
                result_ = fail("Didn't get list back")
            end select
        else
            result_ = fail(results%message%toString())
        end if
    end function checkMany

    pure function checkNone() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: results

        results = many(parseA, newState(var_str("BAA")))
        if (results%ok) then
            result_ = assertThat(results%empty)
        else
            result_ = fail(results%message%toString())
        end if
    end function checkNone

    pure function parseA(state_) result(result_)
        type(State_t), intent(in) :: state_
        type(ParserOutput_t) :: result_

        result_ = parseChar("A", state_)
    end function parseA
end module many_test

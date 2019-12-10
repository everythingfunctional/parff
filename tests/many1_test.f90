module many1_test
    use iso_varying_string, only: var_str
    use parff, only: &
            ParsedItems_t, &
            ParserOutput_t, &
            State_t, &
            many1, &
            newState, &
            parseChar
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertEquals, assertNot, describe, fail, it

    implicit none
    private

    public :: test_many1
contains
    function test_many1() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = it( &
                "parses until the parser doesn't match", checkMany)
        individual_tests(2) = it( &
                "fails if the first result doesn't match", checkNone)
        tests = describe("many1", individual_tests)
    end function test_many1

    pure function checkMany() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: results

        results = many1(parseA, newState(var_str("AAB")))
        if (results%ok) then
            select type (parsed => results%parsed)
            type is (ParsedItems_t)
                result_ = assertEquals(2, size(parsed%items))
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

        results = many1(parseA, newState(var_str("BAA")))
        result_ = assertNot(results%ok, results%message%toString())
    end function checkNone

    pure function parseA(state_) result(result_)
        type(State_t), intent(in) :: state_
        type(ParserOutput_t) :: result_

        result_ = parseChar("A", state_)
    end function parseA
end module many1_test

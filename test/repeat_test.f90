module repeat_test
    use iso_varying_string, only: var_str
    use parff, only: &
            ParsedItems_t, &
            ParserOutput_t, &
            State_t, &
            newState, &
            parseChar, &
            repeat_
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertEquals, assertNot, describe, fail, it

    implicit none
    private

    public :: test_repeat
contains
    function test_repeat() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = it( &
                "uses the parser the given number of times", checkRepeat)
        individual_tests(2) = it( &
                "fails if it can't parse that many", checkNotEnough)
        tests = describe("repeat", individual_tests)
    end function test_repeat

    pure function checkRepeat() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: results

        results = repeat_(parseA, 2, newState(var_str("AA")))
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
    end function checkRepeat

    pure function checkNotEnough() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: results

        results = repeat_(parseA, 3, newState(var_str("AAB")))

        result_ = assertNot(results%ok)
        if (result_%passed()) then
            result_ = &
                    assertEquals("B", results%message%found) &
                    .and.assertEquals("A", results%message%expected(1))
        end if
    end function checkNotEnough

    pure function parseA(state_) result(result_)
        type(State_t), intent(in) :: state_
        type(ParserOutput_t) :: result_

        result_ = parseChar("A", state_)
    end function parseA
end module repeat_test

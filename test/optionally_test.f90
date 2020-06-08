module optionally_test
    use iso_varying_string, only: var_str
    use parff, only: &
            ParsedCharacter_t, &
            ParserOutput_t, &
            State_t, &
            newState, &
            optionally, &
            parseChar
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertEquals, assertThat, describe, fail, it

    implicit none
    private

    public :: test_optionally
contains
    function test_optionally() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = it( &
                "parses the given parser if it succeeds", checkParseSucceed)
        individual_tests(2) = it( &
                "parses nothing if the parser fails", checkParseFails)
        individual_tests(3) = it( &
                "parses nothing if the string is empty", checkParseEmpty)
        tests = describe("optionally", individual_tests)
    end function test_optionally

    pure function checkParseSucceed() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: results

        results = optionally(parseA, newState(var_str("AB")))
        if (results%ok) then
            select type (parsed => results%parsed)
            type is (ParsedCharacter_t)
                result_ = &
                        assertEquals("A", parsed%value_, "parsed") &
                        .and.assertEquals("B", results%remaining, "remaining")
            class default
                result_ = fail("Didn't get the character back")
            end select
        else
            result_ = fail(results%message%toString())
        end if
    end function checkParseSucceed

    pure function checkParseFails() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: results

        results = optionally(parseA, newState(var_str("BB")))
        if (results%ok) then
            result_ = assertThat(results%empty)
        else
            result_ = fail(results%message%toString())
        end if
    end function checkParseFails

    pure function checkParseEmpty() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: results

        results = optionally(parseA, newState(var_str("")))
        if (results%ok) then
            result_ = assertThat(results%empty)
        else
            result_ = fail(results%message%toString())
        end if
    end function checkParseEmpty

    pure function parseA(state_) result(result_)
        type(State_t), intent(in) :: state_
        type(ParserOutput_t) :: result_

        result_ = parseChar("A", state_)
    end function parseA
end module optionally_test

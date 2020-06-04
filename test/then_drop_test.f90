module then_drop_test
    use iso_varying_string, only: var_str
    use parff, only: &
            ParsedCharacter_t, &
            ParserOutput_t, &
            State_t, &
            newState, &
            parseChar, &
            thenDrop
    use Vegetables_m, only: &
            Result_t, &
            TestItem_t, &
            assertEquals, &
            assertNot, &
            assertThat, &
            Describe, &
            fail, &
            It

    implicit none
    private

    public :: test_then_drop
contains
    function test_then_drop() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = It( &
                "When both parses pass, we get the results of the first one", &
                checkBothPass)
        individual_tests(2) = It( &
                "When the first parse fails, that error comes back", &
                checkFirstFail)
        individual_tests(3) = It( &
                "When the second parse fails, that error comes back", &
                checkSecondFail)
        tests = Describe("thenDrop", individual_tests)
    end function test_then_drop

    pure function checkBothPass() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: parse_result

        parse_result = thenDrop(parseA, parseB, newState(var_str("AB")))

        result_ = assertThat(parse_result%ok)
        if (result_%passed()) then
            select type (string => parse_result%parsed)
            type is (ParsedCharacter_t)
                result_ = assertEquals("A", string%value_)
            class default
                result_ = fail("Didn't get character back")
            end select
        end if
    end function checkBothPass

    pure function checkFirstFail() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: parse_result

        parse_result = thenDrop(parseA, parseB, newState(var_str("BB")))

        result_ = assertNot(parse_result%ok)
        if (result_%passed()) then
            result_ = &
                    assertEquals("B", parse_result%message%found) &
                    .and.assertEquals("A", parse_result%message%expected(1))
        end if
    end function checkFirstFail

    pure function checkSecondFail() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: parse_result

        parse_result = thenDrop(parseA, parseB, newState(var_str("AA")))

        result_ = assertNot(parse_result%ok)
        if (result_%passed()) then
            result_ = &
                    assertEquals("A", parse_result%message%found) &
                    .and.assertEquals("B", parse_result%message%expected(1))
        end if
    end function checkSecondFail

    pure function parseA(state_) result(result_)
        type(State_t), intent(in) :: state_
        type(ParserOutput_t) :: result_

        result_ = parseChar("A", state_)
    end function parseA

    pure function parseB(state_) result(result_)
        type(State_t), intent(in) :: state_
        type(ParserOutput_t) :: result_

        result_ = parseChar("B", state_)
    end function parseB
end module then_drop_test

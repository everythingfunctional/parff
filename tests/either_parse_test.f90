module either_parse_test
    use iso_varying_string, only: var_str
    use parff, only: &
            ParsedCharacter_t, &
            ParserOutput_t, &
            State_t, &
            either, &
            newState, &
            parseChar
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

    public :: test_or_parse
contains
    function test_or_parse() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = It( &
                "Returns the first result if it passed", checkFirstPass)
        individual_tests(2) = It( &
                "Returns the second result if it passed", checkSecondPass)
        individual_tests(3) = It( &
                "Returns the error if neither passed", checkBothFail)
        tests = Describe("either", individual_tests)
    end function test_or_parse

    function checkFirstPass() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: parse_result

        parse_result = either(parseF, parseA, newState(var_str("First")))

        result_ = &
                assertThat(parse_result%ok, "Got result", "Didn't get result") &
                .and.assertNot(parse_result%empty, "Wasn't empty", "Was empty")
        if (result_%passed()) then
            select type (the_char => parse_result%parsed)
            type is (ParsedCharacter_t)
                result_ = &
                        assertEquals("F", the_char%value_) &
                        .and.assertEquals("irst", parse_result%remaining)
            class default
                result_ = fail("Didn't get a character back")
            end select
        end if
    end function checkFirstPass

    function checkSecondPass() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: parse_result

        parse_result = either(parseA, parseF, newState(var_str("First")))

        result_ = &
                assertThat(parse_result%ok, "Got result", "Didn't get result") &
                .and.assertNot(parse_result%empty, "Wasn't empty", "Was empty")
        if (result_%passed()) then
            select type (the_char => parse_result%parsed)
            type is (ParsedCharacter_t)
                result_ = &
                        assertEquals("F", the_char%value_) &
                        .and.assertEquals("irst", parse_result%remaining)
            class default
                result_ = fail("Didn't get a character back")
            end select
        end if
    end function checkSecondPass

    function checkBothFail() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: parse_result

        parse_result = either(parseA, parseA, newState(var_str("First")))

        result_ = &
                assertNot(parse_result%ok) &
                .and.assertEquals("F", parse_result%message%found) &
                .and.assertEquals(2, size(parse_result%message%expected))
    end function checkBothFail

    function parseA(state_) result(result_)
        type(State_t), intent(in) :: state_
        type(ParserOutput_t) :: result_

        result_ = parseChar("A", state_)
    end function parseA

    function parseF(state_) result(result_)
        type(State_t), intent(in) :: state_
        type(ParserOutput_t) :: result_

        result_ = parseChar("F", state_)
    end function parseF
end module either_parse_test

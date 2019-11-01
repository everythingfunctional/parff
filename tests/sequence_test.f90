module sequence_test
    implicit none
    private

    public :: test_sequence
contains
    function test_sequence() result(tests)
        use Vegetables_m, only: TestItem_t, Describe, It

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = It( &
                "When both parses pass, the results are combined", &
                checkBothPass)
        individual_tests(2) = It( &
                "When the first parse fails, that error comes back", &
                checkFirstFail)
        individual_tests(3) = It( &
                "When the second parse fails, that error comes back", &
                checkSecondFail)
        tests = Describe("sequence", individual_tests)
    end function test_sequence

    function checkBothPass() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: ParseResult_t, ParsedString_t, newState, sequence
        use Vegetables_m, only: Result_t, assertEquals, assertThat, fail

        type(Result_t) :: result_

        type(ParseResult_t) :: parse_result

        parse_result = sequence(parseA, thenParseB, newState(var_str("AB")))

        result_ = assertThat(parse_result%ok)
        if (result_%passed()) then
            select type (string => parse_result%parsed)
            type is (ParsedString_t)
                result_ = assertEquals("AB", string%value_)
            class default
                result_ = fail("Didn't get string back")
            end select
        end if
    end function checkBothPass

    function checkFirstFail() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: ParseResult_t, ParsedString_t, newState, sequence
        use Vegetables_m, only: Result_t, assertEquals, assertNot, fail

        type(Result_t) :: result_

        type(ParseResult_t) :: parse_result

        parse_result = sequence(parseA, thenParseB, newState(var_str("BB")))

        result_ = assertNot(parse_result%ok)
        if (result_%passed()) then
            result_ = &
                    assertEquals("B", parse_result%message%found) &
                    .and.assertEquals("A", parse_result%message%expected(1))
        end if
    end function checkFirstFail

    function checkSecondFail() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: ParseResult_t, ParsedString_t, newState, sequence
        use Vegetables_m, only: Result_t, assertEquals, assertNot, fail

        type(Result_t) :: result_

        type(ParseResult_t) :: parse_result

        parse_result = sequence(parseA, thenParseB, newState(var_str("AA")))

        result_ = assertNot(parse_result%ok)
        if (result_%passed()) then
            result_ = &
                    assertEquals("A", parse_result%message%found) &
                    .and.assertEquals("B", parse_result%message%expected(1))
        end if
    end function checkSecondFail

    function parseA(state_) result(result_)
        use parff, only: ParseResult_t, State_t, parseChar

        type(State_t), intent(in) :: state_
        type(ParseResult_t) :: result_

        result_ = parseChar("A", state_)
    end function parseA

    function thenParseB(previous, state_) result(result_)
        use iso_varying_string, only: assignment(=)
        use parff, only: &
                ParseResult_t, &
                ParsedCharacter_t, &
                ParsedString_t, &
                ParsedValue_t, &
                State_t, &
                parseChar

        class(ParsedValue_t), intent(in) :: previous
        type(State_t), intent(in) :: state_
        type(ParseResult_t) :: result_

        type(ParsedString_t) :: parsed

        result_ = parseChar("B", state_)

        if (result_%ok) then
            select type (previous)
            type is (ParsedCharacter_t)
                select type (next => result_%parsed)
                type is (ParsedCharacter_t)
                    parsed%value_ = previous%value_ // next%value_
                end select
            end select
            deallocate(result_%parsed)
            allocate(result_%parsed, source = parsed)
        end if
    end function thenParseB
end module sequence_test

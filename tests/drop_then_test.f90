module drop_then_test
    implicit none
    private

    public :: test_drop_then
contains
    function test_drop_then() result(tests)
        use Vegetables_m, only: TestItem_t, Describe, It

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = It( &
                "When both parses pass, we get the results of the second one", &
                checkBothPass)
        individual_tests(2) = It( &
                "When the first parse fails, that error comes back", &
                checkFirstFail)
        individual_tests(3) = It( &
                "When the second parse fails, that error comes back", &
                checkSecondFail)
        tests = Describe("dropThen", individual_tests)
    end function test_drop_then

    function checkBothPass() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: ParserOutput_t, ParsedCharacter_t, dropThen, newState
        use Vegetables_m, only: Result_t, assertEquals, assertThat, fail

        type(Result_t) :: result_

        type(ParserOutput_t) :: parse_result

        parse_result = dropThen(parseA, parseB, newState(var_str("AB")))

        result_ = assertThat(parse_result%ok)
        if (result_%passed()) then
            select type (string => parse_result%parsed)
            type is (ParsedCharacter_t)
                result_ = assertEquals("B", string%value_)
            class default
                result_ = fail("Didn't get character back")
            end select
        end if
    end function checkBothPass

    function checkFirstFail() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: ParserOutput_t, dropThen, newState
        use Vegetables_m, only: Result_t, assertEquals, assertNot, fail

        type(Result_t) :: result_

        type(ParserOutput_t) :: parse_result

        parse_result = dropThen(parseA, parseB, newState(var_str("BB")))

        result_ = assertNot(parse_result%ok)
        if (result_%passed()) then
            result_ = &
                    assertEquals("B", parse_result%message%found) &
                    .and.assertEquals("A", parse_result%message%expected(1))
        end if
    end function checkFirstFail

    function checkSecondFail() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: ParserOutput_t, dropThen, newState
        use Vegetables_m, only: Result_t, assertEquals, assertNot, fail

        type(Result_t) :: result_

        type(ParserOutput_t) :: parse_result

        parse_result = dropThen(parseA, parseB, newState(var_str("AA")))

        result_ = assertNot(parse_result%ok)
        if (result_%passed()) then
            result_ = &
                    assertEquals("A", parse_result%message%found) &
                    .and.assertEquals("B", parse_result%message%expected(1))
        end if
    end function checkSecondFail

    function parseA(state_) result(result_)
        use parff, only: ParserOutput_t, State_t, parseChar

        type(State_t), intent(in) :: state_
        type(ParserOutput_t) :: result_

        result_ = parseChar("A", state_)
    end function parseA

    function parseB(state_) result(result_)
        use parff, only: ParserOutput_t, State_t, parseChar

        type(State_t), intent(in) :: state_
        type(ParserOutput_t) :: result_

        result_ = parseChar("B", state_)
    end function parseB
end module drop_then_test

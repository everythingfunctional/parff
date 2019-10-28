module then_parse_test
    implicit none
    private

    public :: test_then_parse
contains
    function test_then_parse() result(tests)
        use Vegetables_m, only: TestItem_t, Describe, It

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = It( &
                "When both parses pass, the results are combined", &
                checkBothPass)
        individual_tests(2) = It( &
                "When the first parse fails, there are no results", &
                checkFirstFails)
        individual_tests(3) = It( &
                "When the second parse fails, there are no results", &
                checkSecondFails)
        tests = Describe("thenParse", individual_tests)
    end function test_then_parse

    function checkBothPass() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: &
                CharacterParsedValue_t, &
                CombinedParsedValue_t, &
                ParseResults_t, &
                thenParse
        use Vegetables_m, only: Result_t, assertEquals, fail

        type(Result_t) :: result_

        type(ParseResults_t) :: parse_results

        parse_results = thenParse(parseA(var_str("AB")), parseB)

        result_ = assertEquals(1, parse_results%numResults())
        if (result_%passed()) then
            select type (the_results => parse_results%results(1)%parsed_value)
            type is (CombinedParsedValue_t)
                select type (first_result => the_results%first)
                type is (CharacterParsedValue_t)
                    result_ = result_.and.assertEquals("A", first_result%value_)
                class default
                    result_ = result_.and.fail("First result wasn't the character")
                end select
                select type (second_result => the_results%second)
                type is (CharacterParsedValue_t)
                    result_ = result_.and.assertEquals("B", second_result%value_)
                class default
                    result_ = result_.and.fail("Second result wasn't the character")
                end select
            class default
                result_ = result_.and.fail("Didn't get the combined results")
            end select
        end if
    end function checkBothPass

    function checkFirstFails() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: ParseResults_t, thenParse
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(ParseResults_t) :: parse_results

        parse_results = thenParse(parseA(var_str("BB")), parseB)

        result_ = assertEquals(0, parse_results%numResults())
    end function checkFirstFails

    function checkSecondFails() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: ParseResults_t, thenParse
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(ParseResults_t) :: parse_results

        parse_results = thenParse(parseA(var_str("AA")), parseB)

        result_ = assertEquals(0, parse_results%numResults())
    end function checkSecondFails

    function parseA(string) result(results)
        use iso_varying_string, only: VARYING_STRING
        use parff, only: ParseResults_t, parseCharacter

        type(VARYING_STRING), intent(in) :: string
        type(ParseResults_t) :: results

        results = parseCharacter("A", string)
    end function parseA

    function parseB(string) result(results)
        use iso_varying_string, only: VARYING_STRING
        use parff, only: ParseResults_t, parseCharacter

        type(VARYING_STRING), intent(in) :: string
        type(ParseResults_t) :: results

        results = parseCharacter("B", string)
    end function parseB
end module then_parse_test

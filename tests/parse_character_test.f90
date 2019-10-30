module parse_character_test
    implicit none
    private

    public :: test_parse_character
contains
    function test_parse_character() result(tests)
        use Vegetables_m, only: TestItem_t, Describe, It

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = It( &
                "Parsing the first character in a string consumes that character", &
                checkParseFirstCharacter)
        tests = Describe("charP", individual_tests)
    end function test_parse_character

    function checkParseFirstCharacter() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: ParsedCharacter_t, ParseResult_t, charP, newState
        use Vegetables_m, only: &
                Result_t, assertEquals, assertNot, assertThat, fail

        type(Result_t) :: result_

        type(ParseResult_t) :: parse_result

        parse_result = charP("F", newState(var_str("First")))

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
    end function checkParseFirstCharacter
end module parse_character_test

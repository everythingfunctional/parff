module parse_character_test
    use iso_varying_string, only: var_str
    use parff, only: ParsedCharacter_t, ParserOutput_t, newState, parseChar
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

    public :: test_parse_character
contains
    function test_parse_character() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = It( &
                "Parsing the first character in a string consumes that character", &
                checkParseFirstCharacter)
        individual_tests(2) = It( &
                "Parsing a different character produces an error", &
                checkParseDifferentCharacter)
        individual_tests(3) = It( &
                "Parsing an empty string produces an error", &
                checkParseEmptyString)
        tests = Describe("charP", individual_tests)
    end function test_parse_character

    function checkParseFirstCharacter() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: parse_result

        parse_result = parseChar("F", newState(var_str("First")))

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

    function checkParseDifferentCharacter() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: parse_result

        parse_result = parseChar("A", newState(var_str("First")))

        result_ = &
                assertNot(parse_result%ok) &
                .and.assertEquals("F", parse_result%message%found) &
                .and.assertEquals("A", parse_result%message%expected(1))
    end function checkParseDifferentCharacter

    function checkParseEmptyString() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: parse_result

        parse_result = parseChar("A", newState(var_str("")))

        result_ = &
                assertNot(parse_result%ok) &
                .and.assertEquals("end of input", parse_result%message%found) &
                .and.assertEquals("A", parse_result%message%expected(1))
    end function checkParseEmptyString
end module parse_character_test

module parse_character_test
    implicit none
    private

    public :: test_parse_character
contains
    function test_parse_character() result(tests)
        use Vegetables_m, only: TestItem_t, Describe, It

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = It( &
                "Parsing the first character in a string consumes that character", &
                checkParseFirstCharacter)
        individual_tests(2) = It( &
                "Parsing a different character produces no results", &
                checkParseDifferentCharacter)
        individual_tests(3) = It( &
                "Parsing an empty string produces no results", &
                checkParseEmptyString)
        tests = Describe("parseCharacter", individual_tests)
    end function test_parse_character

    function checkParseFirstCharacter() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: CharacterParsedValue_t, ParseResults_t, parseCharacter
        use Vegetables_m, only: Result_t, assertEquals, fail

        type(Result_t) :: result_

        type(ParseResults_t) :: parse_results

        parse_results = parseCharacter("F", var_str("First"))

        result_ = assertEquals(1, parse_results%numResults())
        if (result_%passed()) then
            result_ = result_.and.assertEquals("irst", parse_results%results(1)%remaining)
            select type (the_character => parse_results%results(1)%parsed_value)
            type is (CharacterParsedValue_t)
                result_ = result_.and.assertEquals("F", the_character%value_)
            class default
                result_ = result_.and.fail("Didn't the character back")
            end select
        end if
    end function checkParseFirstCharacter

    function checkParseDifferentCharacter() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: ParseResults_t, parseCharacter
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(ParseResults_t) :: parse_results

        parse_results = parseCharacter("A", var_str("First"))

        result_ = assertEquals(0, parse_results%numResults())
    end function checkParseDifferentCharacter

    function checkParseEmptyString() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: ParseResults_t, parseCharacter
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(ParseResults_t) :: parse_results

        parse_results = parseCharacter("A", var_str(""))

        result_ = assertEquals(0, parse_results%numResults())
    end function checkParseEmptyString
end module parse_character_test

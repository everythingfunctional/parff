module parse_string_test
    implicit none
    private

    public :: test_parse_string
contains
    function test_parse_string() result(tests)
        use Vegetables_m, only: TestItem_t, Describe, It

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(3)

        individual_tests(1) = It( &
                "Parsing the first part of a string consumes that part", &
                checkParseFirstPart)
        individual_tests(2) = It( &
                "Parsing a different string produces no results", &
                checkParseDifferentString)
        individual_tests(3) = It( &
                "Parsing an empty string produces no results", &
                checkParseEmptyString)
        tests = Describe("parseString", individual_tests)
    end function test_parse_string

    function checkParseFirstPart() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: StringParsedValue_t, ParseResults_t, parseString
        use Vegetables_m, only: Result_t, assertEquals, fail

        type(Result_t) :: result_

        type(ParseResults_t) :: parse_results

        parse_results = parseString("First", var_str("FirstSecond"))

        result_ = assertEquals(1, parse_results%numResults())
        if (result_%passed()) then
            result_ = result_.and.assertEquals("Second", parse_results%results(1)%remaining)
            select type (the_string => parse_results%results(1)%parsed_value)
            type is (StringParsedValue_t)
                result_ = result_.and.assertEquals("First", the_string%value_)
            class default
                result_ = result_.and.fail("Didn't get the string back")
            end select
        end if
    end function checkParseFirstPart

    function checkParseDifferentString() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: ParseResults_t, parseString
        use Vegetables_m, only: Result_t, assertEquals, fail

        type(Result_t) :: result_

        type(ParseResults_t) :: parse_results

        parse_results = parseString("Other", var_str("FirstSecond"))

        result_ = assertEquals(0, parse_results%numResults())
    end function checkParseDifferentString

    function checkParseEmptyString() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: ParseResults_t, parseString
        use Vegetables_m, only: Result_t, assertEquals, fail

        type(Result_t) :: result_

        type(ParseResults_t) :: parse_results

        parse_results = parseString("Other", var_str(""))

        result_ = assertEquals(0, parse_results%numResults())
    end function checkParseEmptyString
end module parse_string_test

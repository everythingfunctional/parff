module or_parse_test
    implicit none
    private

    public :: test_or_parse
contains
    function test_or_parse() result(tests)
        use Vegetables_m, only: TestItem_t, Describe, It

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(4)

        individual_tests(1) = It( &
                "Returns the first result if it passed", &
                checkFirstPass)
        individual_tests(2) = It( &
                "Returns the second result if it passed", &
                checkSecondPass)
        individual_tests(3) = It( &
                "Returns both results if they both passed", &
                checkBothPass)
        individual_tests(4) = It( &
                "Returns no results if neither passed", &
                checkNeitherPass)
        tests = Describe("orParse", individual_tests)
    end function test_or_parse

    function checkFirstPass() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: ParseResults_t, operator(.or.), parseCharacter
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(ParseResults_t) :: parse_results

        parse_results = &
                parseCharacter("F", var_str("First")) &
                .or.parseCharacter("A", var_str("First"))

        result_ = assertEquals(1, parse_results%numResults())
    end function checkFirstPass

    function checkSecondPass() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: ParseResults_t, operator(.or.), parseCharacter
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(ParseResults_t) :: parse_results

        parse_results = &
                parseCharacter("A", var_str("First")) &
                .or.parseCharacter("F", var_str("First"))

        result_ = assertEquals(1, parse_results%numResults())
    end function checkSecondPass

    function checkBothPass() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: ParseResults_t, operator(.or.), parseCharacter
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(ParseResults_t) :: parse_results

        parse_results = &
                parseCharacter("F", var_str("First")) &
                .or.parseCharacter("F", var_str("First"))

        result_ = assertEquals(2, parse_results%numResults())
    end function checkBothPass

    function checkNeitherPass() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: ParseResults_t, operator(.or.), parseCharacter
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(ParseResults_t) :: parse_results

        parse_results = &
                parseCharacter("A", var_str("First")) &
                .or.parseCharacter("A", var_str("First"))

        result_ = assertEquals(0, parse_results%numResults())
    end function checkNeitherPass
end module or_parse_test

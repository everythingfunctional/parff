module parse_nothing_test
    implicit none
    private

    public :: test_parse_nothing
contains
    function test_parse_nothing() result(tests)
        use Vegetables_m, only: TestItem_t, Describe, It

        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = It( &
                "leaves the string intact", checkParseNothing)
        tests = Describe("parseNothing", individual_tests)
    end function test_parse_nothing

    function checkParseNothing() result(result_)
        use iso_varying_string, only: var_str
        use parff, only: ParseResults_t, parseNothing
        use Vegetables_m, only: Result_t, assertEquals

        type(Result_t) :: result_

        type(ParseResults_t) :: parse_results

        parse_results = parseNothing(var_str("String"))

        result_ = assertEquals(1, parse_results%numResults())
        if (result_%passed()) then
            result_ = result_.and.assertEquals("String", parse_results%results(1)%remaining)
        end if
    end function checkParseNothing
end module parse_nothing_test

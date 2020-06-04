module parse_string_test
    use iso_varying_string, only: var_str
    use parff, only: ParsedString_t, ParserOutput_t, newState, parseString
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

    public :: test_parse_string
contains
    function test_parse_string() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = It( &
                "Parsing the first part of a string consumes that string", &
                checkPass)
        individual_tests(2) = It( &
                "Parsing something else produces an error", &
                checkFail)
        tests = Describe("parseString", individual_tests)
    end function test_parse_string

    pure function checkPass() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: parse_result

        parse_result = parseString("Hello", newState(var_str("Hello World")))

        result_ = &
                assertThat(parse_result%ok, "Got result", "Didn't get result") &
                .and.assertNot(parse_result%empty, "Wasn't empty", "Was empty")
        if (result_%passed()) then
            select type (the_string => parse_result%parsed)
            type is (ParsedString_t)
                result_ = &
                        assertEquals("Hello", the_string%value_) &
                        .and.assertEquals(" World", parse_result%remaining)
            class default
                result_ = fail("Didn't get a string back")
            end select
        end if
    end function checkPass

    pure function checkFail() result(result_)
        type(Result_t) :: result_

        type(ParserOutput_t) :: parse_result

        parse_result = parseString("Hello", newState(var_str("World")))

        result_ = &
                assertNot(parse_result%ok) &
                .and.assertEquals("W", parse_result%message%found) &
                .and.assertEquals("Hello", parse_result%message%expected(1))
    end function checkFail
end module parse_string_test

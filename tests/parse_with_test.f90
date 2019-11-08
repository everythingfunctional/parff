module parse_with_test
    use parff, only: &
            ParsedCharacter_t, &
            ParseResult_t, &
            ParserOutput_t, &
            State_t, &
            parseChar, &
            parseWith
    use Vegetables_m, only: &
            Result_t, TestItem_t, assertEquals, assertNot, Describe, fail, It

    implicit none
    private

    public :: test_parse_with
contains
    function test_parse_with() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = It( &
                "Gets the parsed result back if successful", &
                checkSuccessful)
        individual_tests(2) = It( &
                "Gets a message back if failed", &
                checkFailure)
        tests = Describe("parseWith", individual_tests)
    end function test_parse_with

    function checkSuccessful() result(result_)
        type(Result_t) :: result_

        type(ParseResult_t) :: the_result

        the_result = parseWith(theParser, "A")

        if (the_result%ok) then
            select type (parsed => the_result%parsed)
            type is (ParsedCharacter_t)
                result_ = assertEquals("A", parsed%value_)
            class default
                result_ = fail("Didn't get a character back")
            end select
        else
            result_ = fail(the_result%message)
        end if
    end function checkSuccessful

    function checkFailure() result(result_)
        type(Result_t) :: result_

        type(ParseResult_t) :: the_result

        the_result = parseWith(theParser, "B")

        result_ = assertNot(the_result%ok, the_result%message)
    end function checkFailure

    function theParser(state) result(result_)
        type(State_t), intent(in) :: state
        type(ParserOutput_t) :: result_

        result_ = parseChar("A", state)
    end function theParser
end module parse_with_test

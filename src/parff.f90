module parff
    use iso_varying_string, only: VARYING_STRING

    implicit none
    private

    type, public, abstract :: ParsedValue_t
    end type ParsedValue_t

    type, public :: ParseResult_t
        class(ParsedValue_t), allocatable :: parsed_value
        type(VARYING_STRING) :: remaining
    end type ParseResult_t

    type, public :: ParseResults_t
        integer :: num_results = 0
        type(ParseResult_t), allocatable :: results(:)
    contains
        private
        procedure, public :: numResults
    end type ParseResults_t

    type, public, extends(ParsedValue_t) :: CharacterParsedValue_t
        character(len=1) :: value_
    end type CharacterParsedValue_t

    type, public, extends(ParsedValue_t) :: StringParsedValue_t
        type(VARYING_STRING) :: value_
    end type StringParsedValue_t

    abstract interface
        function parser(string) result(results)
            use iso_varying_string, only: VARYING_STRING
            import ParseResult_t, ParseResults_t
            type(VARYING_STRING), intent(in) :: string
            type(ParseResults_t) :: results
        end function parser
    end interface

    interface ParseResults
        module procedure ParseResultsSingle
    end interface ParseResults

    interface parseString
        module procedure parseStringC
        module procedure parseStringS
    end interface parseString

    public :: parseCharacter, ParseResult, ParseResults, parseString
contains
    function numResults(self)
        class(ParseResults_t), intent(in) :: self
        integer :: numResults

        numResults = self%num_results
    end function numResults

    function parseCharacter(char_, string) result(results)
        use iso_varying_string, only: VARYING_STRING, len
        use strff, only: firstCharacter, withoutFirstCharacter

        character(len=1), intent(in) :: char_
        type(VARYING_STRING), intent(in) :: string
        type(ParseResults_t) :: results

        type(ParseResult_t) :: the_result
        type(CharacterParsedValue_t) :: parsed_value

        if (len(string) > 0) then
            if (firstCharacter(string) == char_) then
                parsed_value%value_ = char_
                the_result = ParseResult( &
                        parsed_value, withoutFirstCharacter(string))
                results = ParseResults(the_result)
            end if
        end if
    end function parseCharacter

    function ParseResult(parsed_value, remaining)
        use iso_varying_string, only: VARYING_STRING

        class(ParsedValue_t), intent(in) :: parsed_value
        type(VARYING_STRING), intent(in) :: remaining
        type(ParseResult_t) :: ParseResult

        allocate(ParseResult%parsed_value, source = parsed_value)
        ParseResult%remaining = remaining
    end function ParseResult

    function ParseResultsSingle(parse_result)
        type(ParseResult_t), intent(in) :: parse_result
        type(ParseResults_t) :: ParseResultsSingle

        allocate(ParseResultsSingle%results(1))
        ParseResultsSingle%results(1) = parse_result
        ParseResultsSingle%num_results = 1
    end function ParseResultsSingle

    function parseStringC(expected, string) result(results)
        use iso_varying_string, only: VARYING_STRING, var_str

        character(len=*), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: string
        type(ParseResults_t) :: results

        results = parseString(var_str(expected), string)
    end function parseStringC

    function parseStringS(expected, string) result(results)
        use iso_varying_string, only: &
                VARYING_STRING, operator(==), extract, len

        type(VARYING_STRING), intent(in) :: expected
        type(VARYING_STRING), intent(in) :: string
        type(ParseResults_t) :: results

        type(StringParsedValue_t) :: parsed_value
        type(ParseResult_t) :: the_result

        if (expected == extract(string, 1, len(expected))) then
            parsed_value%value_ = expected
            the_result = ParseResult( &
                    parsed_value, extract(string, len(expected) + 1))
            results = ParseResults(the_result)
        end if
    end function parseStringS
end module parff

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

    type, public, extends(ParsedValue_t) :: NothingParsedValue_t
    end type NothingParsedValue_t

    type, public, extends(ParsedValue_t) :: CharacterParsedValue_t
        character(len=1) :: value_
    end type CharacterParsedValue_t

    type, public, extends(ParsedValue_t) :: StringParsedValue_t
        type(VARYING_STRING) :: value_
    end type StringParsedValue_t

    type, public, extends(ParsedValue_t) :: CombinedParsedValue_t
        class(ParsedValue_t), allocatable :: first
        class(ParsedValue_t), allocatable :: second
    end type CombinedParsedValue_t

    abstract interface
        function parser(string) result(results)
            use iso_varying_string, only: VARYING_STRING
            import ParseResult_t, ParseResults_t
            type(VARYING_STRING), intent(in) :: string
            type(ParseResults_t) :: results
        end function parser
    end interface

    interface operator(.or.)
        module procedure orParse
    end interface operator(.or.)

    interface ParseResults
        module procedure ParseResultsSingle
    end interface ParseResults

    interface parseString
        module procedure parseStringC
        module procedure parseStringS
    end interface parseString

    type(NothingParsedValue_t), parameter, public :: NOTHING = NothingParsedValue_t()

    public :: &
            operator(.or.), &
            parseCharacter, &
            parseNothing, &
            ParseResult, &
            ParseResults, &
            parseString, &
            thenParse
contains
    function numResults(self)
        class(ParseResults_t), intent(in) :: self
        integer :: numResults

        numResults = self%num_results
    end function numResults

    function orParse(lhs, rhs) result(combined)
        type(ParseResults_t), intent(in) :: lhs
        type(ParseResults_t), intent(in) :: rhs
        type(ParseResults_t) :: combined

        integer :: i

        if (lhs%num_results > 0) then
            if (rhs%num_results > 0) then
                combined%num_results = lhs%num_results + rhs%num_results
                allocate(combined%results(combined%num_results))
                do i = 1, lhs%num_results
                    combined%results(i) = lhs%results(i)
                end do
                do i = 1, rhs%num_results
                    combined%results(i + lhs%num_results) = rhs%results(i)
                end do
            else
                combined = lhs
            end if
        else
            if (rhs%num_results > 0) then
                combined = rhs
            end if
        end if
    end function orParse

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

    function parseNothing(string) result(results)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: string
        type(ParseResults_t) :: results

        results = ParseResults(ParseResult(NOTHING, string))
    end function parseNothing

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

    function thenParse(previous_results, parse) result(results)
        type(ParseResults_t), intent(in) :: previous_results
        procedure(parser) :: parse
        type(ParseResults_t) :: results

        integer :: i
        type(ParseResults_t) :: intermediate_results
        type(ParseResults_t) :: temp

        do i = 1, previous_results%num_results
            intermediate_results = parse(previous_results%results(i)%remaining)
            if (intermediate_results%num_results > 0) then
                temp = results
                results = addIntermediateResults( &
                        previous_results%results(i)%parsed_value, &
                        temp, &
                        intermediate_results)
            end if
        end do
    contains
        function addIntermediateResults( &
                prior_result, past_parses, new_parses) result(all_parses)
            class(ParsedValue_t), intent(in) :: prior_result
            type(ParseResults_t), intent(in) :: past_parses
            type(ParseResults_t), intent(in) :: new_parses
            type(ParseResults_t) :: all_parses

            type(CombinedParsedValue_t) :: combined_values
            integer :: j

            if (past_parses%num_results == 0) then
                all_parses%num_results = new_parses%num_results
                allocate(all_parses%results(all_parses%num_results))
                do j = 1, new_parses%num_results
                    allocate(combined_values%first, source = prior_result)
                    allocate(combined_values%second, source = new_parses%results(j)%parsed_value)
                    all_parses%results(j) = ParseResult( &
                            combined_values, new_parses%results(j)%remaining)
                end do
            else
                all_parses%num_results = past_parses%num_results + new_parses%num_results
                allocate(all_parses%results(all_parses%num_results))
                do j = 1, past_parses%num_results
                    all_parses%results(j) = past_parses%results(j)
                end do
                do j = 1, new_parses%num_results
                    allocate(combined_values%first, source = prior_result)
                    allocate(combined_values%second, source = new_parses%results(j)%parsed_value)
                    all_parses%results(j + past_parses%num_results) = &
                            ParseResult(combined_values, new_parses%results(j)%remaining)
                end do
            end if
        end function addIntermediateResults
    end function thenParse
end module parff

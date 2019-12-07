module parff
    use iso_varying_string, only: &
            VARYING_STRING, &
            assignment(=), &
            operator(//), &
            operator(==), &
            len, &
            var_str
    use strff, only: &
            operator(.includes.), &
            firstCharacter, &
            join, &
            toString, &
            withoutFirstCharacter, &
            NEWLINE

    implicit none
    private

    type, public, abstract :: ParsedValue_t
    end type ParsedValue_t

    type, public :: ParsedItem_t
        class(ParsedValue_t), allocatable :: item
    end type ParsedItem_t

    type, public, extends(ParsedValue_t) :: ParsedNothing_t
    end type ParsedNothing_t

    type, public, extends(ParsedValue_t) :: ParsedCharacter_t
        character(len=1) :: value_
    end type ParsedCharacter_t

    type, public, extends(ParsedValue_t) :: ParsedString_t
        type(VARYING_STRING) :: value_
    end type ParsedString_t

    type, public, extends(ParsedValue_t) :: IntermediateParsedString_t
        type(VARYING_STRING) :: parsed_so_far
        type(VARYING_STRING) :: left_to_parse
    end type IntermediateParsedString_t

    type, public, extends(ParsedValue_t) :: ParsedItems_t
        type(ParsedItem_t), allocatable :: items(:)
    end type ParsedItems_t

    type, public :: Position_t
        integer :: line
        integer :: column
    end type Position_t

    type, public :: State_t
        type(VARYING_STRING) :: input
        type(Position_t) :: position
    end type State_t

    type, public :: Message_t
        type(Position_t) :: position
        type(VARYING_STRING) :: found
        type(VARYING_STRING), allocatable :: expected(:)
    contains
        procedure :: toString => messageToString
        final :: messageDestructor
    end type Message_t

    type, public :: ParserOutput_t
        logical :: empty
        logical :: ok
        type(Message_t) :: message
        ! The following are only defined if ok
        class(ParsedValue_t), allocatable :: parsed
        type(VARYING_STRING) :: remaining
        type(Position_t) :: position
    end type ParserOutput_t

    type, public :: ParseResult_t
        logical :: ok
        class(ParsedValue_t), allocatable :: parsed
        type(VARYING_STRING) :: message
    end type ParseResult_t

    abstract interface
        pure function match(char_) result(matches)
            character(len=1), intent(in) :: char_
            logical :: matches
        end function match

        pure function parser(state_) result(result_)
            import ParserOutput_t, State_t
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_
        end function parser

        pure function thenParser(previous, state_) result(result_)
            import ParserOutput_t, ParsedValue_t, State_t
            class(ParsedValue_t), intent(in) :: previous
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_
        end function thenParser
    end interface

    interface dropThen
        module procedure dropThenParser
        module procedure dropThenResult
    end interface dropThen

    interface parseString
        module procedure parseStringC
        module procedure parseStringS
    end interface parseString

    interface parseWith
        module procedure parseWithC
        module procedure parseWithS
    end interface parseWith

    interface sequence
        module procedure sequenceParser
        module procedure sequenceResult
    end interface sequence

    interface thenDrop
        module procedure thenDropParser
        module procedure thenDropResult
    end interface thenDrop

    interface withLabel
        module procedure withLabelC
        module procedure withLabelS
    end interface withLabel

    type(ParsedNothing_t), parameter :: PARSED_NOTHING = ParsedNothing_t()

    public :: &
            dropThen, &
            either, &
            many, &
            newState, &
            parseChar, &
            parseDigit, &
            parseNothing, &
            parseString, &
            parseWhitespace, &
            parseWith, &
            return_, &
            satisfy, &
            sequence, &
            thenDrop, &
            withLabel
contains
    pure function ConsumedOk(parsed, remaining, position, message_)
        class(ParsedValue_t), intent(in) :: parsed
        type(VARYING_STRING), intent(in) :: remaining
        type(Position_t), intent(in) :: position
        type(Message_t), intent(in) :: message_
        type(ParserOutput_t) :: ConsumedOk

        ConsumedOk%empty = .false.
        ConsumedOk%ok = .true.
        allocate(ConsumedOk%parsed, source = parsed)
        ConsumedOk%remaining = remaining
        ConsumedOk%position = position
        ConsumedOk%message = message_
    end function ConsumedOk

    pure function dropThenParser(parser1, parser2, state_) result(result_)
        procedure(parser) :: parser1
        procedure(parser) :: parser2
        type(State_t), intent(in) :: state_
        type(ParserOutput_t) :: result_

        result_ = dropThen(parser1(state_), parser2)
    end function dropThenParser

    pure function dropThenResult(previous, parser_) result(result_)
        type(ParserOutput_t), intent(in) :: previous
        procedure(parser) :: parser_
        type(ParserOutput_t) :: result_

        if (previous%ok) then
            result_ = parser_( &
                    State(previous%remaining, previous%position))
            if (.not.previous%empty) then
                result_%empty = .false.
            end if
        else
            result_ = previous
        end if
    end function dropThenResult

    pure function either(parse1, parse2, state_) result(result_)
        procedure(parser) :: parse1
        procedure(parser) :: parse2
        type(State_t), intent(in) :: state_
        type(ParserOutput_t) :: result_

        type(ParserOutput_t) :: first_result
        type(ParserOutput_t) :: second_result

        first_result = parse1(state_)

        if (first_result%empty) then
            second_result = parse2(state_)
            if (second_result%empty) then
                if (first_result%ok) then
                    result_ = mergeOk( &
                            first_result%parsed, &
                            first_result%remaining, &
                            first_result%position, &
                            first_result%message, &
                            second_result%message)
                else
                    if (second_result%ok) then
                        result_ = mergeOk( &
                                second_result%parsed, &
                                second_result%remaining, &
                                second_result%position, &
                                first_result%message, &
                                second_result%message)
                    else
                        result_ = mergeError( &
                                first_result%message, &
                                second_result%message)
                    end if
                end if
            else
                result_ = second_result
            end if
        else
            result_ = first_result
        end if
    end function either

    pure function EmptyError(message_)
        type(Message_t), intent(in) :: message_
        type(ParserOutput_t) :: EmptyError

        EmptyError%empty = .true.
        EmptyError%ok = .false.
        EmptyError%message = message_
    end function EmptyError

    pure function EmptyOk(parsed, remaining, position, message_)
        class(ParsedValue_t), intent(in) :: parsed
        type(VARYING_STRING), intent(in) :: remaining
        type(Position_t), intent(in) :: position
        type(Message_t), intent(in) :: message_
        type(ParserOutput_t) :: EmptyOk

        EmptyOk%empty = .true.
        EmptyOk%ok = .true.
        allocate(EmptyOk%parsed, source = parsed)
        EmptyOk%remaining = remaining
        EmptyOk%position = position
        EmptyOk%message = message_
    end function EmptyOk

    pure function expect(message_, label) result(new_message)
        type(Message_t), intent(in) :: message_
        type(VARYING_STRING), intent(in) :: label
        type(Message_t) :: new_message

        new_message = Message(message_%position, message_%found, [label])
    end function expect

    pure function many(the_parser, the_state) result(the_result)
        procedure(parser) :: the_parser
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        type(ParsedItems_t) :: start

        allocate(start%items(0))
        the_result = recurse(start, the_state)
    contains
        pure recursive function recurse(previous, state_) result(result_)
            class(ParsedValue_t), intent(in) :: previous
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            type(ParsedItems_t) :: all
            type(ParserOutput_t) :: next
            type(ParsedItem_t) :: the_item

            select type (previous)
            type is (ParsedItems_t)
                next = the_parser(state_)
                if (next%ok) then
                    allocate(the_item%item, source = next%parsed)
                    allocate(all%items, source = [previous%items,the_item])
                    result_ = recurse(all, State(next%remaining, next%position))
                else
                    if (size(previous%items) == 0) then
                        result_ = EmptyOk( &
                                previous, &
                                state_%input, &
                                state_%position, &
                                Message( &
                                        state_%position, &
                                        var_str(""), &
                                        [VARYING_STRING::]))
                    else
                        result_ = ConsumedOk( &
                                previous, &
                                state_%input, &
                                state_%position, &
                                Message( &
                                        state_%position, &
                                        var_str(""), &
                                        [VARYING_STRING::]))
                    end if
                end if
            end select
        end function recurse
    end function many

    pure function merge_(message1, message2) result(merged)
        type(Message_t), intent(in) :: message1
        type(Message_t), intent(in) :: message2
        type(Message_t) :: merged

        merged = Message( &
                message1%position, &
                message1%found, &
                [message1%expected, message2%expected])
    end function merge_

    pure function mergeError(message1, message2) result(result_)
        type(Message_t), intent(in) :: message1
        type(Message_t), intent(in) :: message2
        type(ParserOutput_t) :: result_

        result_ = EmptyError(merge_(message1, message2))
    end function mergeError

    pure function mergeOk( &
            parsed, remaining, position, message1, message2) result(result_)
        class(ParsedValue_t), intent(in) :: parsed
        type(VARYING_STRING), intent(in) :: remaining
        type(Position_t), intent(in) :: position
        type(Message_t), intent(in) :: message1
        type(Message_t), intent(in) :: message2
        type(ParserOutput_t) :: result_

        result_ = EmptyOk( &
                parsed, &
                remaining, &
                position, &
                merge_(message1, message2))
    end function mergeOk

    pure function Message(position, found, expected)
        type(Position_t), intent(in) :: position
        type(VARYING_STRING), intent(in) :: found
        type(VARYING_STRING), intent(in) :: expected(:)
        type(Message_t) :: Message

        Message%position = position
        Message%found = found
        allocate(Message%expected, source = expected)
    end function Message

    pure subroutine messageDestructor(self)
        type(Message_t), intent(inout) :: self

        if(allocated(self%expected)) deallocate(self%expected)
    end subroutine messageDestructor

    pure function messageToString(self) result(string)
        class(Message_t), intent(in) :: self
        type(VARYING_STRING) :: string

        string = "At line " // toString(self%position%line) // " and column " // toString(self%position%column) // NEWLINE &
                // "    found " // self%found // " but expected " // join(self%expected, " or ")
    end function messageToString

    pure function newPosition()
        type(Position_t) :: newPosition

        newPosition%line = 1
        newPosition%column = 1
    end function newPosition

    pure function newState(input)
        type(VARYING_STRING), intent(in) :: input
        type(State_t) :: newState

        newState = State(input, newPosition())
    end function newState

    pure function nextPosition(char_, position)
        character(len=1), intent(in) :: char_
        type(Position_t), intent(in) :: position
        type(Position_t) :: nextPosition

        character(len=1), parameter :: TAB = char(z'0009')
        character(len=1), parameter :: NEWLINE_ = char(z'000A')

        if (char_ == NEWLINE_) then
            nextPosition%line = position%line + 1
            nextPosition%column = position%column
        else if (char_ == TAB) then
            nextPosition%line = position%line
            nextPosition%column = position%column + 8 - mod(position%column - 1, 8)
        else
            nextPosition%line = position%line
            nextPosition%column = position%column + 1
        end if
    end function nextPosition

    pure function parseChar(the_char, the_state) result(the_result)
        character(len=1), intent(in) :: the_char
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        the_result = withLabel(the_char, theParser, the_state)
    contains
        pure function theParser(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = satisfy(theMatcher, state_)
        end function theParser

        pure function theMatcher(char_) result(matches)
            character(len=1), intent(in) :: char_
            logical :: matches

            matches = char_ == the_char
        end function theMatcher
    end function parseChar

    pure function parseDigit(the_state) result(the_result)
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        the_result = withLabel("digit", theParser, the_state)
    contains
        pure function theParser(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = satisfy(theMatcher, state_)
        end function theParser

        pure function theMatcher(char_) result(matches)
            character(len=1), intent(in) :: char_
            logical :: matches

            matches = "0123456789".includes.char_
        end function theMatcher
    end function parseDigit

    pure function parseNothing(the_state) result(the_result)
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        the_result = return_(PARSED_NOTHING, the_state)
    end function parseNothing

    pure function parseStringC(string, the_state) result(the_result)
        character(len=*), intent(in) :: string
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        the_result = parseString(var_str(string), the_state)
    end function parseStringC

    pure function parseStringS(string, the_state) result(the_result)
        type(VARYING_STRING), intent(in) :: string
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        the_result = withLabel(string, start, the_state)
    contains
        pure function start(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            type(ParsedString_t) :: empty
            type(IntermediateParsedString_t) :: initial

            if (string == "") then
                empty%value_ = ""
                result_ = EmptyOk(empty, state_%input, state_%position, Message( &
                        state_%position, var_str(""), [VARYING_STRING::]))
            else
                initial%left_to_parse = string
                initial%parsed_so_far = ""
                result_ = sequence(return_(initial, state_), recurse)
            end if
        end function start

        pure recursive function recurse(previous, state_) result(result_)
            class(ParsedValue_t), intent(in) :: previous
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            type(ParsedString_t) :: final_string

            select type (previous)
            type is (IntermediateParsedString_t)
                if (len(previous%left_to_parse) == 0) then
                    final_string%value_ = previous%parsed_so_far
                    result_ = ConsumedOk( &
                            final_string, &
                            state_%input, &
                            state_%position, &
                            Message(state_%position, var_str(""), [VARYING_STRING::]))
                else
                    result_ = sequence(parseNext(previous, state_), recurse)
                end if
            end select
        end function recurse

        pure function parseNext(previous, state_) result(result_)
            type(IntermediateParsedString_t), intent(in) :: previous
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            type(IntermediateParsedString_t) :: next

            result_ = parseChar(firstCharacter(previous%left_to_parse), state_)
            if (result_%ok) then
                next%left_to_parse = withoutFirstCharacter(previous%left_to_parse)
                select type (the_char => result_%parsed)
                type is (ParsedCharacter_t)
                    next%parsed_so_far = previous%parsed_so_far // the_char%value_
                    deallocate(result_%parsed)
                    allocate(result_%parsed, source = next)
                end select
            end if
        end function parseNext
    end function parseStringS

    pure function parseWhitespace(the_state) result(the_result)
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        the_result = withLabel("whitespace", theParser, the_state)
    contains
        pure function theParser(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = satisfy(theMatcher, state_)
        end function theParser

        pure function theMatcher(char_) result(matches)
            character(len=1), intent(in) :: char_
            logical :: matches

            character(len=1), parameter :: TAB = char(z'0009')
            character(len=1), parameter :: CARRIAGE_RETURN = char(z'000D')
            character(len=1), parameter :: SPACE = char(z'0020')
            character(len=*), parameter :: WHITESPACE = &
                    TAB // NEWLINE // CARRIAGE_RETURN // SPACE

            matches = WHITESPACE.includes.char_
        end function theMatcher
    end function parseWhitespace

    pure function parseWithC(theParser, string) result(result_)
        procedure(parser) :: theParser
        character(len=*), intent(in) :: string
        type(ParseResult_t) :: result_

        result_ = parseWith(theParser, var_str(string))
    end function parseWithC

    pure function parseWithS(theParser, string) result(result_)
        procedure(parser) :: theParser
        type(VARYING_STRING), intent(in) :: string
        type(ParseResult_t) :: result_

        type(ParserOutput_t) :: the_results

        the_results = theParser(newState(string))
        if (the_results%ok) then
            result_%ok = .true.
            allocate(result_%parsed, source = the_results%parsed)
        else
            result_%ok = .false.
            result_%message = the_results%message%toString()
        end if
    end function parseWithS

    pure function return_(parsed, state_) result(result_)
        class(ParsedValue_t), intent(in) :: parsed
        type(State_t), intent(in) :: state_
        type(ParserOutput_t) :: result_

        result_ = EmptyOk( &
                parsed, state_%input, state_%position, Message( &
                        state_%position, var_str(""), [VARYING_STRING::]))
    end function return_

    pure function satisfy(matches, state_) result(result_)
        procedure(match) :: matches
        type(State_t), intent(in) :: state_
        type(ParserOutput_t) :: result_

        character(len=1) :: first_character
        type(Position_t) :: new_position
        type(ParsedCharacter_t) :: parsed_character

        if (len(state_%input) > 0) then
            first_character = firstCharacter(state_%input)
            if (matches(first_character)) then
                new_position = nextPosition(first_character, state_%position)
                parsed_character%value_ = first_character
                result_ = ConsumedOk( &
                        parsed_character, &
                        withoutFirstCharacter(state_%input), &
                        new_position, &
                        Message( &
                                new_position, &
                                var_str(""), &
                                [VARYING_STRING::]))
            else
                result_ = EmptyError(Message( &
                        state_%position, &
                        var_str(first_character), &
                        [VARYING_STRING::]))
            end if
        else
            result_ = EmptyError(Message( &
                    state_%position, &
                    var_str("end of input"), &
                    [VARYING_STRING::]))
        end if
    end function satisfy

    pure function sequenceParser(parser1, parser2, state_) result(result_)
        procedure(parser) :: parser1
        procedure(thenParser) :: parser2
        type(State_t), intent(in) :: state_
        type(ParserOutput_t) :: result_

        result_ = sequence(parser1(state_), parser2)
    end function sequenceParser

    pure function sequenceResult(previous, parser_) result(result_)
        type(ParserOutput_t), intent(in) :: previous
        procedure(thenParser) :: parser_
        type(ParserOutput_t) :: result_

        if (previous%ok) then
            result_ = parser_( &
                    previous%parsed, &
                    State(previous%remaining, previous%position))
            if (.not.previous%empty) then
                result_%empty = .false.
            end if
        else
            result_ = previous
        end if
    end function sequenceResult

    pure function State(input, position)
        type(VARYING_STRING), intent(in) :: input
        type(Position_t), intent(in) :: position
        type(State_t) :: State

        State%input = input
        State%position = position
    end function State

    pure function thenDropParser(parser1, parser2, state_) result(result_)
        procedure(parser) :: parser1
        procedure(parser) :: parser2
        type(State_t), intent(in) :: state_
        type(ParserOutput_t) :: result_

        result_ = thenDrop(parser1(state_), parser2)
    end function thenDropParser

    pure function thenDropResult(previous, parser_) result(result_)
        type(ParserOutput_t), intent(in) :: previous
        procedure(parser) :: parser_
        type(ParserOutput_t) :: result_

        if (previous%ok) then
            result_ = parser_( &
                    State(previous%remaining, previous%position))
            result_%empty = previous%empty .and. result_%empty
            if (result_%ok) then
                deallocate(result_%parsed)
                allocate(result_%parsed, source = previous%parsed)
            end if
        else
            result_ = previous
        end if
    end function thenDropResult

    pure function withLabelC(label, parse, state_) result(result_)
        character(len=*), intent(in) :: label
        procedure(parser) :: parse
        type(State_t), intent(in) :: state_
        type(ParserOutput_t) :: result_

        result_ = withLabel(var_str(label), parse, state_)
    end function withLabelC

    pure function withLabelS(label, parse, state_) result(result_)
        type(VARYING_STRING), intent(in) :: label
        procedure(parser) :: parse
        type(State_t), intent(in) :: state_
        type(ParserOutput_t) :: result_

        type(ParserOutput_t) :: the_result
        type(Message_t) :: the_message

        the_result = parse(state_)
        if (the_result%empty) then
            if (the_result%ok) then
                the_message = expect(the_result%message, label)
                result_ = EmptyOk( &
                        the_result%parsed, &
                        the_result%remaining, &
                        the_result%position, &
                        the_message)
            else
                the_message = expect(the_result%message, label)
                result_ = EmptyError(the_message)
            end if
        else
            result_ = the_result
        end if
    end function withLabelS
end module parff

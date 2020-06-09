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

    type, public, extends(ParsedValue_t) :: ParsedInteger_t
        integer :: value_
    end type ParsedInteger_t

    type, public, extends(ParsedValue_t) :: ParsedRational_t
        double precision :: value_
    end type ParsedRational_t

    type, public, extends(ParsedValue_t) :: IntermediateParsedString_t
        type(VARYING_STRING) :: parsed_so_far
        type(VARYING_STRING) :: left_to_parse
    end type IntermediateParsedString_t

    type, public, extends(ParsedValue_t) :: ParsedItems_t
        type(ParsedItem_t), allocatable :: items(:)
    contains
        final :: parsedItemsDestructor
    end type ParsedItems_t

    type, public, extends(ParsedValue_t) :: IntermediateRepeat_t
        type(ParsedItems_t) :: parsed_so_far
        integer :: remaining
    end type IntermediateRepeat_t

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
            ConsumedOk, &
            dropThen, &
            either, &
            EmptyError, &
            EmptyOk, &
            many, &
            many1, &
            many1WithSeparator, &
            manyWithSeparator, &
            Message, &
            newState, &
            optionally, &
            parseChar, &
            parseDigit, &
            parseInteger, &
            parseNothing, &
            parseRational, &
            parseString, &
            parseWhitespace, &
            parseWith, &
            repeat_, &
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

        the_result = manyWithSeparator(the_parser, parseNothing, the_state)
    end function many

    pure function many1(the_parser, the_state) result(the_result)
        procedure(parser) :: the_parser
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        the_result = many1WithSeparator(the_parser, parseNothing, the_state)
    end function many1

    pure function many1WithSeparator( &
            the_parser, the_separator, the_state) result(the_result)
        procedure(parser) :: the_parser
        procedure(parser) :: the_separator
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        type(ParsedItems_t) :: all
        type(ParserOutput_t) :: next
        type(ParsedItems_t) :: temp

        the_result = the_parser(the_state)
        if (the_result%ok) then
            allocate(all%items(1))
            allocate(all%items(1)%item, source = the_result%parsed)
            do
                next = dropThen(the_separator, the_parser, State(the_result%remaining, the_result%position))
                if (.not.next%ok) exit
                allocate(temp%items(size(all%items)))
                temp%items = all%items
                deallocate(all%items)
                allocate(all%items(size(temp%items) + 1))
                all%items(1:size(temp%items)) = temp%items
                allocate(all%items(size(all%items))%item, source = next%parsed)
                deallocate(temp%items)
                the_result = next
            end do
            deallocate(the_result%parsed)
            allocate(the_result%parsed, source = all)
        end if
    end function many1WithSeparator

    pure function manyWithSeparator( &
            the_parser, the_separator, the_state) result(the_result)
        procedure(parser) :: the_parser
        procedure(parser) :: the_separator
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        type(ParsedItems_t) :: all

        the_result = many1WithSeparator(the_parser, the_separator, the_state)
        if (.not.the_result%ok) then
            allocate(all%items(0))
            the_result = EmptyOk( &
                    all, &
                    the_state%input, &
                    the_state%position, &
                    Message( &
                            the_state%position, &
                            var_str(""), &
                            [VARYING_STRING::]))
        end if
    end function manyWithSeparator

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

        character(len=1), parameter :: TAB = char(9)
        character(len=1), parameter :: NEWLINE_ = char(10)

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

    pure function optionally(the_parser, the_state) result(the_result)
        procedure(parser) :: the_parser
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        the_result = either(the_parser, parseNothing, the_state)
    end function optionally

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

    pure function parseInteger(the_state) result(the_result)
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        the_result = withLabel("integer", theParser, the_state)
    contains
        pure function theParser(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            integer :: the_number
            character(len=64) :: the_string
            type(ParsedInteger_t) :: the_value

            result_ = sequence(optionally(parseSign, state_), thenParseDigits)
            if (result_%ok) then
                select type (parsed_string => result_%parsed)
                type is (ParsedString_t)
                    the_string = parsed_string%value_
                    read(the_string, *) the_number
                    the_value%value_ = the_number
                    deallocate(result_%parsed)
                    allocate(result_%parsed, source = the_value)
                end select
            end if
        end function theParser

        pure function parseSign(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = either(parsePlus, parseMinus, state_)
        end function parseSign

        pure function parsePlus(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = parseChar("+", state_)
        end function parsePlus

        pure function parseMinus(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = parseChar("-", state_)
        end function parseMinus

        pure function thenParseDigits(previous, state_) result(result_)
            class(ParsedValue_t), intent(in) :: previous
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = parseDigits(state_)
            if (result_%ok) then
                select type (previous)
                type is (ParsedCharacter_t)
                    select type (next => result_%parsed)
                    type is (ParsedString_t)
                        next%value_ = previous%value_ // next%value_
                    end select
                end select
            end if
        end function thenParseDigits

        pure function parseDigits(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            type(VARYING_STRING), allocatable :: digits(:)
            integer :: i
            type(ParsedString_t) :: parsed_digits

            result_ = many1(parseDigit, state_)
            if (result_%ok) then
                select type (results => result_%parsed)
                type is (ParsedItems_t)
                    allocate(digits(size(results%items)))
                    do i = 1, size(digits)
                        select type (string => results%items(i)%item)
                        type is (ParsedCharacter_t)
                            digits(i) = string%value_
                        end select
                    end do
                end select
                deallocate(result_%parsed)
                parsed_digits%value_ = join(digits, "")
                allocate(result_%parsed, source = parsed_digits)
            end if
        end function parseDigits
    end function parseInteger

    pure subroutine parsedItemsDestructor(self)
        type(ParsedItems_t), intent(inout) :: self

        if (allocated(self%items)) deallocate(self%items)
    end subroutine parsedItemsDestructor

    pure function parseNothing(the_state) result(the_result)
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        the_result = return_(PARSED_NOTHING, the_state)
    end function parseNothing

    pure function parseRational(the_state) result(the_result)
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        the_result = withLabel("rational", theParser, the_state)
    contains
        pure function theParser(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            double precision :: the_number
            character(len=64) :: the_string
            type(ParsedRational_t) :: the_value

            result_ = sequence( &
                    sequence(parseSign, thenParseNumber, state_), &
                    thenParseExponent)
            if (result_%ok) then
                select type (parsed_string => result_%parsed)
                type is (ParsedString_t)
                    the_string = parsed_string%value_
                    read(the_string, *) the_number
                    the_value%value_ = the_number
                    deallocate(result_%parsed)
                    allocate(result_%parsed, source = the_value)
                end select
            end if
        end function theParser

        pure function parseSign(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            type(ParsedString_t) :: the_string

            result_ = either(parsePlus, parseMinus, state_)
            if (result_%ok) then
                select type (the_character => result_%parsed)
                type is (ParsedCharacter_t)
                    the_string%value_ = the_character%value_
                    deallocate(result_%parsed)
                    allocate(result_%parsed, source = the_string)
                end select
            else
                the_string%value_ = ""
                result_ = EmptyOk( &
                    the_string, &
                    state_%input, &
                    state_%position, &
                    Message(state_%position, var_str(""), [VARYING_STRING::]))
            end if
        end function parseSign

        pure function parsePlus(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = parseChar("+", state_)
        end function parsePlus

        pure function parseMinus(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = parseChar("-", state_)
        end function parseMinus

        pure function thenParseNumber(previous, state_) result(result_)
            class(ParsedValue_t), intent(in) :: previous
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = either(parseCoveredDecimal, parseUncoveredDecimal, state_)
            if (result_%ok) then
                select type (previous)
                type is (ParsedString_t)
                    select type (next => result_%parsed)
                    type is (ParsedString_t)
                        next%value_ = previous%value_ // next%value_
                    end select
                end select
            end if
        end function thenParseNumber

        pure function parseCoveredDecimal(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = sequence(parseDigits, thenParseFraction, state_)
        end function parseCoveredDecimal

        pure function parseDigits(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            type(VARYING_STRING), allocatable :: digits(:)
            integer :: i
            type(ParsedString_t) :: parsed_digits

            result_ = many1(parseDigit, state_)
            if (result_%ok) then
                select type (results => result_%parsed)
                type is (ParsedItems_t)
                    allocate(digits(size(results%items)))
                    do i = 1, size(digits)
                        select type (string => results%items(i)%item)
                        type is (ParsedCharacter_t)
                            digits(i) = string%value_
                        end select
                    end do
                    deallocate(result_%parsed)
                    parsed_digits%value_ = join(digits, "")
                    allocate(result_%parsed, source = parsed_digits)
                end select
            end if
        end function parseDigits

        pure function thenParseFraction(previous, state_) result(result_)
            class(ParsedValue_t), intent(in) :: previous
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = sequence(parseDecimal, thenParseMaybeDigits, state_)
            if (result_%ok) then
                select type (previous)
                type is (ParsedString_t)
                    select type (next => result_%parsed)
                    type is (ParsedString_t)
                        next%value_ = previous%value_ // next%value_
                    end select
                end select
            else
                result_ = EmptyOk( &
                        previous, &
                        state_%input, &
                        state_%position, &
                        Message(state_%position, var_str(""), [VARYING_STRING::]))
            end if
        end function thenParseFraction

        pure function parseDecimal(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            type(ParsedString_t) :: the_string

            result_ = parseChar(".", state_)
            if (result_%ok) then
                select type (the_character => result_%parsed)
                type is (ParsedCharacter_t)
                    the_string%value_ = the_character%value_
                    deallocate(result_%parsed)
                    allocate(result_%parsed, source = the_string)
                end select
            end if
        end function parseDecimal

        pure function thenParseMaybeDigits(previous, state_) result(result_)
            class(ParsedValue_t), intent(in) :: previous
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = parseMaybeDigits(state_)
            if (result_%ok) then
                select type (previous)
                type is (ParsedString_t)
                    select type (next => result_%parsed)
                    type is (ParsedString_t)
                        next%value_ = previous%value_ // next%value_
                    end select
                end select
            end if
        end function thenParseMaybeDigits

        pure function parseMaybeDigits(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            type(VARYING_STRING), allocatable :: digits(:)
            integer :: i
            type(ParsedString_t) :: parsed_digits

            result_ = many(parseDigit, state_)
            select type (results => result_%parsed)
            type is (ParsedItems_t)
                allocate(digits(size(results%items)))
                do i = 1, size(digits)
                    select type (string => results%items(i)%item)
                    type is (ParsedCharacter_t)
                        digits(i) = string%value_
                    end select
                end do
                deallocate(result_%parsed)
                parsed_digits%value_ = join(digits, "")
                allocate(result_%parsed, source = parsed_digits)
            end select
        end function parseMaybeDigits

        pure function parseUncoveredDecimal(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = sequence(parseDecimal, thenParseDigits, state_)
        end function parseUncoveredDecimal

        pure function thenParseDigits(previous, state_) result(result_)
            class(ParsedValue_t), intent(in) :: previous
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = parseDigits(state_)
            if (result_%ok) then
                select type (previous)
                type is (ParsedString_t)
                    select type (next => result_%parsed)
                    type is (ParsedString_t)
                        next%value_ = previous%value_ // next%value_
                    end select
                end select
            end if
        end function thenParseDigits

        pure function thenParseExponent(previous, state_) result(result_)
            class(ParsedValue_t), intent(in) :: previous
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = parseExponent(state_)
            if (result_%ok) then
                select type (previous)
                type is (ParsedString_t)
                    select type (next => result_%parsed)
                    type is (ParsedString_t)
                        next%value_ = previous%value_ // next%value_
                    end select
                end select
            else
                result_ = EmptyOk( &
                        previous, &
                        state_%input, &
                        state_%position, &
                        Message(state_%position, var_str(""), [VARYING_STRING::]))
            end if
        end function thenParseExponent

        pure function parseExponent(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = sequence( &
                    sequence(parseLetter, thenParseSign, state_), &
                    thenParseDigits)
        end function parseExponent

        pure function parseLetter(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            type(ParsedString_t) :: the_string

            result_ = either(parseE, parseD, state_)
            if (result_%ok) then
                select type (the_character => result_%parsed)
                type is (ParsedCharacter_t)
                    the_string%value_ = the_character%value_
                    deallocate(result_%parsed)
                    allocate(result_%parsed, source = the_string)
                end select
            end if
        end function parseLetter

        pure function parseE(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = either(parseUpperE, parseLowerE, state_)
        end function parseE

        pure function parseUpperE(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = parseChar("E", state_)
        end function parseUpperE

        pure function parseLowerE(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = parseChar("e", state_)
        end function parseLowerE

        pure function parseD(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = either(parseUpperD, parseLowerD, state_)
        end function parseD

        pure function parseUpperD(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = parseChar("D", state_)
        end function parseUpperD

        pure function parseLowerD(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = parseChar("d", state_)
        end function parseLowerD

        pure function thenParseSign(previous, state_) result(result_)
            class(ParsedValue_t), intent(in) :: previous
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            result_ = parseSign(state_)
            if (result_%ok) then
                select type (previous)
                type is (ParsedString_t)
                    select type (next => result_%parsed)
                    type is (ParsedString_t)
                        next%value_ = previous%value_ // next%value_
                    end select
                end select
            end if
        end function thenParseSign
    end function parseRational

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

            character(len=1), parameter :: TAB = char(9)
            character(len=1), parameter :: CARRIAGE_RETURN = char(13)
            character(len=1), parameter :: SPACE = char(32)
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

    pure function repeat_(the_parser, times, the_state) result(the_result)
        procedure(parser) :: the_parser
        integer, intent(in) :: times
        type(State_t), intent(in) :: the_state
        type(ParserOutput_t) :: the_result

        the_result = start(the_state)
    contains
        pure function start(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            type(ParsedItems_t) :: empty
            type(IntermediateRepeat_t) :: initial

            if (times <= 0) then
                allocate(empty%items, source = [ParsedItem_t::])
                result_ = EmptyOk(empty, state_%input, state_%position, Message( &
                        state_%position, var_str(""), [VARYING_STRING::]))
            else
                initial%remaining = times
                allocate(initial%parsed_so_far%items, source = [ParsedItem_t::])
                result_ = sequence(return_(initial, state_), recurse)
            end if
        end function start

        pure recursive function recurse(previous, state_) result(result_)
            class(ParsedValue_t), intent(in) :: previous
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            type(ParsedItems_t) :: final_list

            select type (previous)
            type is (IntermediateRepeat_t)
                if (previous%remaining <= 0) then
                    allocate(final_list%items, source =  &
                            previous%parsed_so_far%items)
                    result_ = ConsumedOk( &
                            final_list, &
                            state_%input, &
                            state_%position, &
                            Message(state_%position, var_str(""), [VARYING_STRING::]))
                else
                    result_ = sequence(parseNext(previous, state_), recurse)
                end if
            end select
        end function recurse

        pure function parseNext(previous, state_) result(result_)
            type(IntermediateRepeat_t), intent(in) :: previous
            type(State_t), intent(in) :: state_
            type(ParserOutput_t) :: result_

            type(IntermediateRepeat_t) :: next
            type(ParsedItem_t) :: this_item

            result_ = the_parser(state_)
            if (result_%ok) then
                next%remaining = previous%remaining - 1
                allocate(this_item%item, source = result_%parsed)
                allocate(next%parsed_so_far%items, source = [previous%parsed_so_far%items, this_item])
                deallocate(result_%parsed)
                allocate(result_%parsed, source = next)
            end if
        end function parseNext
    end function repeat_

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

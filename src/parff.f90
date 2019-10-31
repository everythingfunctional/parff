module parff
    use iso_varying_string, only: VARYING_STRING

    implicit none
    private

    type, public, abstract :: ParsedValue_t
    end type ParsedValue_t

    type, public, extends(ParsedValue_t) :: ParsedCharacter_t
        character(len=1) :: value_
    end type ParsedCharacter_t

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
        final :: messageDestructor
    end type Message_t

    type, public :: ParseResult_t
        logical :: empty
        logical :: ok
        type(Message_t) :: message
        ! The following are only defined if ok
        class(ParsedValue_t), allocatable :: parsed
        type(VARYING_STRING) :: remaining
        type(Position_t) :: position
    end type ParseResult_t

    abstract interface
        function match(char_) result(matches)
            character(len=1), intent(in) :: char_
            logical :: matches
        end function match

        function parser(state_) result(result_)
            import ParseResult_t, State_t
            type(State_t), intent(in) :: state_
            type(ParseResult_t) :: result_
        end function parser

        ! function thenParser(previous, state) result(consumed)
        !     import Consumed_t, ParsedValue_t, State_t
        !     class(ParsedValue_t), intent(in) :: previous
        !     type(State_t), intent(in) :: state
        !     type(Consumed_t) :: consumed
        ! end function thenParser
    end interface

    public :: charP, either, newState
contains
    function charP(the_char, the_state) result(the_result)
        use iso_varying_string, only: VARYING_STRING, len, var_str
        use strff, only: firstCharacter, withoutFirstCharacter

        character(len=1), intent(in) :: the_char
        type(State_t), intent(in) :: the_state
        type(ParseResult_t) :: the_result

        the_result = withLabel(var_str(the_char), theParser, the_state)
    contains
        function theParser(state_) result(result_)
            type(State_t), intent(in) :: state_
            type(ParseResult_t) :: result_

            result_ = satisfy(theMatcher, state_)
        end function theParser

        function theMatcher(char_) result(matches)
            character(len=1), intent(in) :: char_
            logical :: matches

            matches = char_ == the_char
        end function theMatcher
    end function charP

    function ConsumedOk(parsed, remaining, position, message_)
        class(ParsedValue_t), intent(in) :: parsed
        type(VARYING_STRING), intent(in) :: remaining
        type(Position_t), intent(in) :: position
        type(Message_t), intent(in) :: message_
        type(ParseResult_t) :: ConsumedOk

        ConsumedOk%empty = .false.
        ConsumedOk%ok = .true.
        allocate(ConsumedOk%parsed, source = parsed)
        ConsumedOk%remaining = remaining
        ConsumedOk%position = position
        ConsumedOk%message = message_
    end function ConsumedOk

    function EmptyError(message_)
        type(Message_t), intent(in) :: message_
        type(ParseResult_t) :: EmptyError

        EmptyError%empty = .true.
        EmptyError%ok = .false.
        EmptyError%message = message_
    end function EmptyError

    function EmptyOk(parsed, remaining, position, message_)
        class(ParsedValue_t), intent(in) :: parsed
        type(VARYING_STRING), intent(in) :: remaining
        type(Position_t), intent(in) :: position
        type(Message_t), intent(in) :: message_
        type(ParseResult_t) :: EmptyOk

        EmptyOk%empty = .true.
        EmptyOk%ok = .true.
        allocate(EmptyOk%parsed, source = parsed)
        EmptyOk%remaining = remaining
        EmptyOk%position = position
        EmptyOk%message = message_
    end function EmptyOk

    function expect(message_, label) result(new_message)
        use iso_varying_string, only: VARYING_STRING

        type(Message_t), intent(in) :: message_
        type(VARYING_STRING), intent(in) :: label
        type(Message_t) :: new_message

        new_message = Message(message_%position, message_%found, [label])
    end function expect

    function Message(position, found, expected)
        use iso_varying_string, only: VARYING_STRING

        type(Position_t), intent(in) :: position
        type(VARYING_STRING), intent(in) :: found
        type(VARYING_STRING), intent(in) :: expected(:)
        type(Message_t) :: Message

        Message%position = position
        Message%found = found
        allocate(Message%expected, source = expected)
    end function Message

    subroutine messageDestructor(self)
        type(Message_t), intent(inout) :: self

        if(allocated(self%expected)) deallocate(self%expected)
    end subroutine messageDestructor

    function newPosition()
        type(Position_t) :: newPosition

        newPosition%line = 1
        newPosition%column = 1
    end function newPosition

    function newState(input)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: input
        type(State_t) :: newState

        newState = State(input, newPosition())
    end function newState

    function nextPosition(char_, position)
        character(len=1), intent(in) :: char_
        type(Position_t), intent(in) :: position
        type(Position_t) :: nextPosition

        character(len=1), parameter :: TAB = char(z'0009')
        character(len=1), parameter :: NEWLINE = char(z'000A')

        if (char_ == NEWLINE) then
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

    function State(input, position)
        use iso_varying_string, only: VARYING_STRING

        type(VARYING_STRING), intent(in) :: input
        type(Position_t), intent(in) :: position
        type(State_t) :: State

        State%input = input
        State%position = position
    end function State

    function either(parse1, parse2, state_) result(result_)
        procedure(parser) :: parse1
        procedure(parser) :: parse2
        type(State_t), intent(in) :: state_
        type(ParseResult_t) :: result_

        type(ParseResult_t) :: first_result
        type(ParseResult_t) :: second_result

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

    function merge_(message1, message2) result(merged)
        type(Message_t), intent(in) :: message1
        type(Message_t), intent(in) :: message2
        type(Message_t) :: merged

        merged = Message( &
                message1%position, &
                message1%found, &
                [message1%expected, message2%expected])
    end function merge_

    function mergeError(message1, message2) result(result_)
        type(Message_t), intent(in) :: message1
        type(Message_t), intent(in) :: message2
        type(ParseResult_t) :: result_

        result_ = EmptyError(merge_(message1, message2))
    end function mergeError

    function mergeOk( &
            parsed, remaining, position, message1, message2) result(result_)
        class(ParsedValue_t), intent(in) :: parsed
        type(VARYING_STRING), intent(in) :: remaining
        type(Position_t), intent(in) :: position
        type(Message_t), intent(in) :: message1
        type(Message_t), intent(in) :: message2
        type(ParseResult_t) :: result_

        result_ = EmptyOk( &
                parsed, &
                remaining, &
                position, &
                merge_(message1, message2))
    end function mergeOk
    !
    function satisfy(matches, state_) result(result_)
        use iso_varying_string, only: VARYING_STRING, len, var_str
        use strff, only: firstCharacter, withoutFirstCharacter

        procedure(match) :: matches
        type(State_t), intent(in) :: state_
        type(ParseResult_t) :: result_

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
    !
    ! function sequence(parser1, parser2, state_) result(consumed_)
    !     procedure(parser) :: parser1
    !     procedure(thenParser) :: parser2
    !     type(State_t), intent(in) :: state_
    !     type(Consumed_t) :: consumed_
    !
    !     type(Consumed_t) :: first_result
    !     type(Consumed_t) :: second_result
    !
    !     first_result = parser1(state_)
    !     if (first_result%reply%ok) then
    !         if (first_result%empty) then
    !             consumed_ = parser2( &
    !                     first_result%reply%parsed, first_result%reply%state)
    !         else
    !             second_result = parser2( &
    !                     first_result%reply%parsed, first_result%reply%state)
    !             consumed_ = Consumed(second_result%reply)
    !         end if
    !     else
    !         consumed_ = first_result
    !     end if
    ! end function sequence
    !
    !
    function withLabel(label, parse, state_) result(result_)
        use iso_varying_string, only: VARYING_STRING, char

        type(VARYING_STRING), intent(in) :: label
        procedure(parser) :: parse
        type(State_t), intent(in) :: state_
        type(ParseResult_t) :: result_

        type(ParseResult_t) :: the_result
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
    end function withLabel
end module parff

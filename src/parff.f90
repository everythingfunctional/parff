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
    !
    ! type, public :: Reply_t
    !     logical :: ok
    !     class(ParsedValue_t), allocatable :: parsed
    !     type(State_t) :: state
    !     type(Message_t) :: message
    ! end type Reply_t
    !
    ! type, public :: Consumed_t
    !     logical :: empty
    !     type(Reply_t) :: reply
    ! end type Consumed_t
    !
    ! abstract interface
    !     function match(char_) result(matches)
    !         character(len=1), intent(in) :: char_
    !         logical :: matches
    !     end function match
    !
    !     function parser(state) result(consumed)
    !         import Consumed_t, State_t
    !         type(State_t), intent(in) :: state
    !         type(Consumed_t) :: consumed
    !     end function parser

        ! function thenParser(previous, state) result(consumed)
        !     import Consumed_t, ParsedValue_t, State_t
        !     class(ParsedValue_t), intent(in) :: previous
        !     type(State_t), intent(in) :: state
        !     type(Consumed_t) :: consumed
        ! end function thenParser
!     end interface
!
    public :: charP, newState
contains
    function charP(the_char, the_state) result(result_)
        use iso_varying_string, only: VARYING_STRING, len, var_str
        use strff, only: firstCharacter, withoutFirstCharacter

        character(len=1), intent(in) :: the_char
        type(State_t), intent(in) :: the_state
        type(ParseResult_t) :: result_

        type(VARYING_STRING) :: expected
        character(len=1) :: first_character
        type(Position_t) :: new_position
        type(VARYING_STRING) :: remaining
        type(ParsedCharacter_t) :: the_character

        if (len(the_state%input) > 0) then
            first_character = firstCharacter(the_state%input)
            if (first_character == the_char) then
                result_%empty = .false.
                result_%ok = .true.
                the_character%value_ = the_char
                allocate(result_%parsed, source = the_character)
                remaining = withoutFirstCharacter(the_state%input)
                new_position = nextPosition(the_char, the_state%position)
                result_%remaining = remaining
                result_%position = new_position
                result_%message = Message( &
                        the_state%position, var_str(""), [VARYING_STRING::])
            else
                result_%empty = .true.
                result_%ok = .false.
                expected = var_str(the_char)
                result_%message = Message( &
                        the_state%position, &
                        var_str(first_character), &
                        [expected])
            end if
        else
            result_%empty = .true.
            result_%ok = .false.
            expected = var_str(the_char)
            result_%message = Message( &
                    the_state%position, &
                    var_str("end of input"), &
                    [expected])
        end if
    end function charP

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

!     function charP(the_char, the_state) result(consumed_)
!         use iso_varying_string, only: var_str
!
!         character(len=1), intent(in) :: the_char
!         type(State_t), intent(in) :: the_state
!         type(Consumed_t) :: consumed_
!
!         consumed_ = withLabel(var_str(the_char), theParser, the_state)
!     contains
!         function theParser(state_) result(the_result)
!             type(State_t), intent(in) :: state_
!             type(Consumed_t) :: the_result
!
!             the_result = satisfy(theMatcher, state)
!         end function theParser
!
!         function theMatcher(char_) result(matches)
!             character(len=1), intent(in) :: char_
!             logical :: matches
!
!             matches = char_ == the_char
!         end function theMatcher
!     end function charP
    !
    ! function Consumed(reply)
    !     type(Reply_t), intent(in) :: reply
    !     type(Consumed_t) :: Consumed
    !
    !     Consumed%empty = .false.
    !     Consumed%reply = reply
    ! end function Consumed
    !
    ! function either(parse1, parse2, state_) result(consumed_)
    !     procedure(parser) :: parse1
    !     procedure(parser) :: parse2
    !     type(State_t), intent(in) :: state_
    !     type(Consumed_t) :: consumed_
    !
    !     type(Consumed_t) :: first_result
    !     type(Consumed_t) :: second_result
    !
    !     first_result = parse1(state_)
    !
    !     if (first_result%empty) then
    !         second_result = parse2(state_)
    !         if (second_result%empty) then
    !             if (first_result%reply%ok) then
    !                 consumed_ = mergeOk( &
    !                         first_result%reply%parsed, &
    !                         first_result%reply%state, &
    !                         first_result%reply%message, &
    !                         second_result%reply%message)
    !             else
    !                 if (second_result%reply%ok) then
    !                     consumed_ = mergeOk( &
    !                             second_result%reply%parsed, &
    !                             second_result%reply%state, &
    !                             first_result%reply%message, &
    !                             second_result%reply%message)
    !                 else
    !                     consumed_ = mergeError( &
    !                             first_result%reply%message, &
    !                             second_result%reply%message)
    !                 end if
    !             end if
    !         else
    !             consumed_ = second_result
    !         end if
    !     else
    !         consumed_ = first_result
    !     end if
    ! end function either

    ! function Empty(reply)
    !     type(Reply_t), intent(in) :: reply
    !     type(Consumed_t) :: Empty
    !
    !     Empty%empty = .true.
    !     Empty%reply = reply
    ! end function Empty
    !
    ! function Error(message_)
    !     type(Message_t), intent(in) :: message_
    !     type(Reply_t) :: Error
    !
    !     Error%ok = .false.
    !     Error%message = message_
    ! end function Error
    !
    ! function expect(message_, label) result(new_message)
    !     use iso_varying_string, only: VARYING_STRING
    !
    !     type(Message_t), intent(in) :: message_
    !     type(VARYING_STRING), intent(in) :: label
    !     type(Message_t) :: new_message
    !
    !     new_message = Message(message_%position, message_%found, [label])
    ! end function expect
    !
    ! function Ok(parsed, state_, message_)
    !     class(ParsedValue_t), intent(in) :: parsed
    !     type(State_t), intent(in) :: state_
    !     type(Message_t), intent(in) :: message_
    !     type(Reply_t) :: Ok
    !
    !     allocate(Ok%parsed, source = parsed)
    !     Ok%state = state_
    !     Ok%ok = .true.
    !     Ok%message = message_
    ! end function Ok

    ! function merge_(message1, message2) result(merged)
    !     type(Message_t), intent(in) :: message1
    !     type(Message_t), intent(in) :: message2
    !     type(Message_t) :: merged
    !
    !     merged = Message( &
    !             message1%position, &
    !             message1%found, &
    !             [message1%expected, message2%expected])
    ! end function merge_
    !
    ! function mergeError(message1, message2) result(consumed_)
    !     type(Message_t), intent(in) :: message1
    !     type(Message_t), intent(in) :: message2
    !     type(Consumed_t) :: consumed_
    !
    !     consumed_ = Empty(Error(merge_(message1, message2)))
    ! end function mergeError
    !
    ! function mergeOk(parsed, state_, message1, message2) result(consumed_)
    !     class(ParsedValue_t), intent(in) :: parsed
    !     type(State_t), intent(in) :: state_
    !     type(Message_t), intent(in) :: message1
    !     type(Message_t), intent(in) :: message2
    !     type(Consumed_t) :: consumed_
    !
    !     consumed_ = Empty(Ok(parsed, state, merge_(message1, message2)))
    ! end function mergeOk
    !
    ! function satisfy(matches, state_) result(consumed_)
    !     use iso_varying_string, only: VARYING_STRING, len, var_str
    !     use strff, only: firstCharacter, withoutFirstCharacter
    !
    !     procedure(match) :: matches
    !     type(State_t), intent(in) :: state_
    !     type(Consumed_t) :: consumed_
    !
    !     character(len=1) :: first_character
    !     type(Position_t) :: new_position
    !     type(State_t) :: new_state
    !     type(ParsedCharacter_t) :: parsed_character
    !
    !     if (len(state_%input) > 0) then
    !         first_character = firstCharacter(state_%input)
    !         if (matches(first_character)) then
    !             new_position = nextPosition(first_character, state_%position)
    !             new_state = State(withoutFirstCharacter(state_%input), new_position)
    !             parsed_character%value_ = first_character
    !             consumed_ = Consumed(Ok( &
    !                     parsed_character, &
    !                     new_state, &
    !                     Message( &
    !                             new_position, &
    !                             var_str(""), &
    !                             [VARYING_STRING::])))
    !         else
    !             consumed_ = Empty(Error(Message( &
    !                     state_%position, &
    !                     var_str(first_character), &
    !                     [VARYING_STRING::])))
    !         end if
    !     else
    !         consumed_ = Empty(Error(Message( &
    !                 state_%position, &
    !                 var_str("end of input"), &
    !                 [VARYING_STRING::])))
    !     end if
    ! end function satisfy
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
    ! function withLabel(label, parse, state_) result(consumed_)
    !     use iso_varying_string, only: VARYING_STRING
    !
    !     type(VARYING_STRING), intent(in) :: label
    !     procedure(parser) :: parse
    !     type(State_t), intent(in) :: state_
    !     type(Consumed_t) :: consumed_
    !
    !     type(Consumed_t) :: the_result
    !     type(Message_t) :: the_message
    !     type(Reply_t) :: the_reply
    !
    !     the_result = parse(state_)
    !     if (the_result%empty) then
    !         if (the_result%reply%ok) then
    !             the_message = expect(the_result%reply%message, label)
    !             the_reply = Ok( &
    !                     the_result%reply%parsed, &
    !                     the_result%reply%state, &
    !                     the_message)
    !             consumed_ = Empty(the_reply)
    !         else
    !             the_message = expect(the_result%reply%message, label)
    !             the_reply = Error(the_message)
    !             consumed_ = Empty(the_reply)
    !         end if
    !     else
    !         consumed_ = the_result
    !     end if
    ! end function withLabel
end module parff

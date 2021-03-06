module parff_parser_output_m
    use iso_varying_string, only: varying_string
    use parff_message_m, only: message_t, merge_
    use parff_parsed_value_m, only: parsed_value_t
    use parff_position_m, only: position_t

    implicit none
    private
    public :: &
            parser_output_t, &
            consumed_ok, &
            empty_error, &
            empty_ok, &
            merge_error, &
            merge_ok

    type :: parser_output_t
        logical :: empty
        logical :: ok
        type(message_t) :: message
        ! The following are only defined if ok
        class(parsed_value_t), allocatable :: parsed
        type(varying_string) :: remaining
        type(position_t) :: position
    contains
        private
        procedure, public :: but_not_empty
        procedure, public :: with_parsed_value
    end type
contains
    function consumed_ok(parsed, remaining, position, message)
        class(parsed_value_t), intent(in) :: parsed
        type(varying_string), intent(in) :: remaining
        type(position_t), intent(in) :: position
        type(message_t), intent(in) :: message
        type(parser_output_t) :: consumed_ok

        consumed_ok%empty = .false.
        consumed_ok%ok = .true.
        allocate(consumed_ok%parsed, source = parsed)
        consumed_ok%remaining = remaining
        consumed_ok%position = position
        consumed_ok%message = message
    end function

    function empty_error(message)
        type(message_t), intent(in) :: message
        type(parser_output_t) :: empty_error

        empty_error%empty = .true.
        empty_error%ok = .false.
        empty_error%message = message
    end function

    function empty_ok(parsed, remaining, position, message)
        class(parsed_value_t), intent(in) :: parsed
        type(varying_string), intent(in) :: remaining
        type(position_t), intent(in) :: position
        type(message_t), intent(in) :: message
        type(parser_output_t) :: empty_ok

        empty_ok%empty = .true.
        empty_ok%ok = .true.
        allocate(empty_ok%parsed, source = parsed)
        empty_ok%remaining = remaining
        empty_ok%position = position
        empty_ok%message = message
    end function

    function merge_error(message1, message2) result(result_)
        type(message_t), intent(in) :: message1
        type(message_t), intent(in) :: message2
        type(parser_output_t) :: result_

        result_ = empty_error(merge_(message1, message2))
    end function

    function merge_ok( &
            parsed, remaining, position, message1, message2) result(result_)
        class(parsed_value_t), intent(in) :: parsed
        type(varying_string), intent(in) :: remaining
        type(position_t), intent(in) :: position
        type(message_t), intent(in) :: message1
        type(message_t), intent(in) :: message2
        type(parser_output_t) :: result_

        result_ = empty_ok( &
                parsed, &
                remaining, &
                position, &
                merge_(message1, message2))
    end function

    subroutine but_not_empty(self)
        class(parser_output_t), intent(inout) :: self

        self%empty = .false.
    end subroutine

    function with_parsed_value(self, parsed)
        class(parser_output_t), intent(in) :: self
        class(parsed_value_t), intent(in) :: parsed
        type(parser_output_t) :: with_parsed_value

        if (.not. self%ok) then
            error stop "parff_parser_output_m|with_parsed_value: attempted to add parsed value to failed output"
        else
            with_parsed_value%ok = self%ok
            with_parsed_value%empty = self%empty
            with_parsed_value%message = self%message
            allocate(with_parsed_value%parsed, source = parsed)
            with_parsed_value%remaining = self%remaining
            with_parsed_value%position = self%position
        end if
    end function
end module

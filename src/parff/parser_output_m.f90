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
        private
        logical :: empty_
        logical :: ok_
        type(message_t) :: message_
        ! The following are only defined if ok
        class(parsed_value_t), allocatable :: parsed_
        type(varying_string) :: remaining_
        type(position_t) :: position_
    contains
        private
        procedure, public :: empty
        procedure, public :: ok
        procedure, public :: message
        procedure, public :: parsed
        procedure, public :: remaining
        procedure, public :: position
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

        consumed_ok%empty_ = .false.
        consumed_ok%ok_ = .true.
        allocate(consumed_ok%parsed_, source = parsed)
        consumed_ok%remaining_ = remaining
        consumed_ok%position_ = position
        consumed_ok%message_ = message
    end function

    function empty_error(message)
        type(message_t), intent(in) :: message
        type(parser_output_t) :: empty_error

        empty_error%empty_ = .true.
        empty_error%ok_ = .false.
        empty_error%message_ = message
    end function

    function empty_ok(parsed, remaining, position, message)
        class(parsed_value_t), intent(in) :: parsed
        type(varying_string), intent(in) :: remaining
        type(position_t), intent(in) :: position
        type(message_t), intent(in) :: message
        type(parser_output_t) :: empty_ok

        empty_ok%empty_ = .true.
        empty_ok%ok_ = .true.
        allocate(empty_ok%parsed_, source = parsed)
        empty_ok%remaining_ = remaining
        empty_ok%position_ = position
        empty_ok%message_ = message
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

    function but_not_empty(self)
        class(parser_output_t), intent(in) :: self
        type(parser_output_t) :: but_not_empty

        but_not_empty%ok_ = self%ok_
        but_not_empty%message_ = self%message_
        but_not_empty%empty_ = .false.
        if (self%ok_) then
            allocate(but_not_empty%parsed_, source = self%parsed_)
            but_not_empty%remaining_ = self%remaining_
            but_not_empty%position_ = self%position_
        end if
    end function

    function with_parsed_value(self, parsed)
        class(parser_output_t), intent(in) :: self
        class(parsed_value_t), intent(in) :: parsed
        type(parser_output_t) :: with_parsed_value

        if (.not. self%ok_) then
            error stop "parff_parser_output_m|with_parsed_value: attempted to add parsed value to failed output"
        else
            with_parsed_value%ok_ = self%ok_
            with_parsed_value%empty_ = self%empty_
            with_parsed_value%message_ = self%message_
            allocate(with_parsed_value%parsed_, source = parsed)
            with_parsed_value%remaining_ = self%remaining_
            with_parsed_value%position_ = self%position_
        end if
    end function

    pure function empty(self)
        class(parser_output_t), intent(in) :: self
        logical :: empty

        empty = self%empty_
    end function

    pure function ok(self)
        class(parser_output_t), intent(in) :: self
        logical :: ok

        ok = self%ok_
    end function

    pure function message(self)
        class(parser_output_t), intent(in) :: self
        type(message_t) :: message

        message = self%message_
    end function

    function parsed(self)
        class(parser_output_t), intent(in) :: self
        class(parsed_value_t), allocatable :: parsed

        allocate(parsed, source = self%parsed_)
    end function

    pure function remaining(self)
        class(parser_output_t), intent(in) :: self
        type(varying_string) :: remaining

        remaining = self%remaining_
    end function

    pure function position(self)
        class(parser_output_t), intent(in) :: self
        type(position_t) :: position

        position = self%position_
    end function
end module

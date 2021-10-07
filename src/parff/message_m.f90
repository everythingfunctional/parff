module parff_message_m
    use iso_varying_string, only: varying_string, operator(//)
    use parff_position_m, only: position_t
    use strff, only: join, to_string, NEWLINE

    implicit none
    private
    public :: message_t, expect, merge_

    type :: message_t
        type(position_t) :: position
        type(varying_string) :: found
        type(varying_string), allocatable :: expected(:)
    contains
        private
        procedure, public :: to_string => message_to_string
    end type
contains
    pure function message_to_string(self) result(string)
        class(message_t), intent(in) :: self
        type(varying_string) :: string

        string = "At line " // to_string(self%position%line) // " and column " // to_string(self%position%column) // NEWLINE &
                // "    found " // self%found // " but expected " // join(self%expected, " or ")
    end function

    pure function merge_(message1, message2) result(merged)
        type(message_t), intent(in) :: message1
        type(message_t), intent(in) :: message2
        type(message_t) :: merged

        merged = message_t( &
                message1%position, &
                message1%found, &
                [message1%expected, message2%expected])
    end function

    pure function expect(message, label) result(new_message)
        type(message_t), intent(in) :: message
        type(varying_string), intent(in) :: label
        type(message_t) :: new_message

        new_message = message_t(message%position, message%found, [label])
    end function
end module

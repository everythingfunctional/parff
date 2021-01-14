module parff_message_m
    use iso_varying_string, only: varying_string, operator(//)
    use parff_position_m, only: position_t
    use strff, only: join, to_string, NEWLINE

    implicit none
    private
    public :: message_t, expect, merge_

    type :: message_t
        private
        type(position_t) :: position_
        type(varying_string) :: found_
        type(varying_string), allocatable :: expected_(:)
    contains
        private
        procedure, public :: to_string => message_to_string
        procedure, public :: found
        procedure, public :: expected
    end type

    interface message_t
        module procedure constructor
    end interface
contains
    pure function constructor(position, found, expected) result(message)
        type(position_t), intent(in) :: position
        type(varying_string), intent(in) :: found
        type(varying_string), intent(in) :: expected(:)
        type(message_t) :: message

        message%position_ = position
        message%found_ = found
        allocate(message%expected_, source = expected)
    end function

    pure function message_to_string(self) result(string)
        class(message_t), intent(in) :: self
        type(varying_string) :: string

        string = "At line " // to_string(self%position_%line()) // " and column " // to_string(self%position_%column()) // NEWLINE &
                // "    found " // self%found_ // " but expected " // join(self%expected_, " or ")
    end function

    pure function merge_(message1, message2) result(merged)
        type(message_t), intent(in) :: message1
        type(message_t), intent(in) :: message2
        type(message_t) :: merged

        merged = message_t( &
                message1%position_, &
                message1%found_, &
                [message1%expected_, message2%expected_])
    end function

    pure function expect(message, label) result(new_message)
        type(message_t), intent(in) :: message
        type(varying_string), intent(in) :: label
        type(message_t) :: new_message

        new_message = message_t(message%position_, message%found_, [label])
    end function

    pure function found(self)
        class(message_t), intent(in) :: self
        type(varying_string) :: found

        found = self%found_
    end function

    pure function expected(self)
        class(message_t), intent(in) :: self
        type(varying_string), allocatable :: expected(:)

        allocate(expected, source = self%expected_)
    end function
end module
